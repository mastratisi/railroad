#!/usr/bin/env bash
# Checks the dependency bounds in railroad.cabal and widens upper bounds.
#
#   1. Runs the tests with the oldest versions the bounds allow.
#   2. Runs the tests with the newest versions the bounds allow.
#   3. For each dependency with a release above its upper bound
#      (per `cabal outdated`), runs the tests against that release.
#      If they pass, the upper bound is raised to the next major version.
#
#   4. If a newer GHC series than those in .github/ghc-versions.json has been
#      released, installs it with ghcup and runs the tests. If they pass, adds
#      the series to that file and raises the `base` upper bound if needed.
#      Skipped when ghcup is not available.
#
# `base` is skipped in step 3: it is tied to the GHC, so step 4 handles it.
#
# When any bound is widened, it also bumps the last component of the package
# version and adds a CHANGELOG.md entry.
#
# Usage: scripts/check-bounds.sh [--dry-run]
#   --dry-run   report what would change, but do not edit railroad.cabal
set -uo pipefail

cd "$(dirname "$0")/.."
cabal_file=railroad.cabal
changelog=CHANGELOG.md
ghc_file=.github/ghc-versions.json
dry_run=false
[ "${1:-}" = "--dry-run" ] && dry_run=true

failed=false
widened=()
rejected=()

run_tests() { # <builddir> <extra cabal flags...>
  local dir=$1; shift
  cabal test --builddir="$dir" --enable-tests "$@"
}

echo "== Oldest allowed versions"
run_tests dist-bounds-oldest --prefer-oldest || { echo "FAIL: oldest"; failed=true; }

echo "== Newest allowed versions"
run_tests dist-bounds-newest || { echo "FAIL: newest"; failed=true; }

if $failed; then
  echo "Tests fail within the current bounds; fix those before widening."
  exit 1
fi

# upper_bound 2.7.1.0 -> 2.8   (PVP: bump the second component)
upper_bound() {
  local IFS=.
  read -r -a v <<< "$1"
  if [ "${#v[@]}" -ge 2 ]; then echo "${v[0]}.$((v[1] + 1))"; else echo "$((v[0] + 1))"; fi
}

echo "== Newer releases outside the bounds"
cabal update >/dev/null
# lines look like: "    * effectful >=2.5 && <2.7 (latest: 2.7.1.0)"
mapfile -t outdated < <(
  cabal outdated 2>/dev/null |
    sed -n 's/^ *\* \([^ ]*\) .*(latest: \([^)]*\)).*/\1 \2/p' |
    sort -u -k1,1
)

for entry in "${outdated[@]}"; do
  read -r pkg latest <<< "$entry"
  if [ "$pkg" = base ]; then
    echo "-- base $latest: skipped (depends on GHC)"
    continue
  fi
  echo "-- $pkg $latest"
  dir=dist-bounds-$pkg
  if run_tests "$dir" --allow-newer="$pkg" --constraint="$pkg == $latest" &&
     grep -q "\"$pkg-$latest-" "$dir/cache/plan.json"; then
    new=$(upper_bound "$latest")
    echo "   passes; new upper bound: < $new"
    widened+=("$pkg < $new")
    if ! $dry_run; then
      # replace the last "< X" on the package's line(s)
      sed -i "/^ *$pkg[ \t]/ s/\(.*< *\)[0-9.]*\(.*\)/\1$new\2/" "$cabal_file"
    fi
  else
    echo "   FAILED (or $latest could not be used); bound unchanged"
    rejected+=("$pkg $latest")
  fi
done

# latest released GHC, e.g. 9.14.1 (skips dated alpha directories like 9.12.20241014)
latest_ghc() {
  curl -fsS https://downloads.haskell.org/ghc/ |
    grep -oE 'href="[0-9]+\.[0-9]+\.[0-9]{1,2}/"' | sed 's/href="//;s/\/"//' |
    sort -V | tail -1
}

# version_gt A B: true when A > B
version_gt() { [ "$1" != "$2" ] && [ "$(printf '%s\n%s\n' "$1" "$2" | sort -V | tail -1)" = "$1" ]; }

echo "== Newer GHC"
ghc_added=""
if ! command -v ghcup >/dev/null; then
  echo "-- ghcup not found; skipping"
elif ! latest=$(latest_ghc) || [ -z "$latest" ]; then
  echo "-- could not determine the latest GHC; skipping"
else
  series=${latest%.*}
  if grep -q "\"$series\"" "$ghc_file"; then
    echo "-- GHC $latest: series $series already in $ghc_file"
  elif ! ghcup install ghc "$latest"; then
    echo "-- GHC $latest: ghcup cannot install it yet; skipping"
  else
    echo "-- GHC $latest"
    base_ver=$("ghc-pkg-$latest" field base version --simple-output)
    if run_tests "dist-bounds-ghc-$latest" -w "ghc-$latest" --allow-newer=base; then
      echo "   passes (base $base_ver)"
      ghc_added=$series
      cur=$(sed -n 's/^ *base *>= *[0-9.]* *&& *< *\([0-9.]*\).*/\1/p' "$cabal_file" | head -1)
      new=$(upper_bound "$base_ver")
      if version_gt "$new" "$cur"; then
        widened+=("base < $new (GHC $series)")
        if ! $dry_run; then
          sed -i "/^ *base[ \t]/ s/\(.*< *\)[0-9.]*\(.*\)/\1$new\2/" "$cabal_file"
        fi
      fi
      if ! $dry_run; then
        sed -i "s/\]\$/, \"$series\"]/" "$ghc_file"
      fi
    else
      echo "   FAILED; GHC $series not added"
      rejected+=("ghc $latest")
    fi
  fi
fi

echo
echo "== Summary"
[ ${#widened[@]} -gt 0 ]  && printf 'widened:  %s\n' "${widened[@]}"
[ ${#rejected[@]} -gt 0 ] && printf 'rejected: %s\n' "${rejected[@]}"
[ -n "$ghc_added" ] && echo "added GHC $ghc_added to $ghc_file"
[ ${#widened[@]} -eq 0 ] && [ ${#rejected[@]} -eq 0 ] && [ -z "$ghc_added" ] && echo "bounds are up to date"
if [ ${#widened[@]} -gt 0 ] && ! $dry_run; then
  old=$(sed -n 's/^version: *//p' "$cabal_file")
  new="${old%.*}.$((${old##*.} + 1))"
  sed -i "s/^\(version: *\)$old\$/\1$new/" "$cabal_file"

  entry="## $new -- $(date +%F)"$'\n'"* Widen dependency upper bounds (tests pass):"
  for w in "${widened[@]}"; do entry+=$'\n'"  - \`$w\`"; done
  # insert after the "# Revision history" title and its blank line
  { head -n 2 "$changelog"; printf '%s\n\n' "$entry"; tail -n +3 "$changelog"; } > "$changelog.tmp"
  mv "$changelog.tmp" "$changelog"

  echo "Updated $cabal_file ($old -> $new) and $changelog. Review with git diff."
fi
exit 0
