#!/usr/bin/env bash
# Checks the dependency bounds in railroad.cabal and widens upper bounds.
#
#   1. Runs the tests with the oldest versions the bounds allow.
#   2. Runs the tests with the newest versions the bounds allow.
#   3. For each dependency with a release above its upper bound
#      (per `cabal outdated`), runs the tests against that release.
#      If they pass, the upper bound is raised to the next major version.
#
# `base` is skipped in step 3: it is tied to the installed GHC, so check it
# by running this script (or the CI matrix) under each GHC instead.
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
      sed -i "s/\(\b$pkg *>= *[0-9.]* *&& *< *\)[0-9.]*/\1$new/" "$cabal_file"
    fi
  else
    echo "   FAILED (or $latest could not be used); bound unchanged"
    rejected+=("$pkg $latest")
  fi
done

echo
echo "== Summary"
[ ${#widened[@]} -gt 0 ]  && printf 'widened:  %s\n' "${widened[@]}"
[ ${#rejected[@]} -gt 0 ] && printf 'rejected: %s\n' "${rejected[@]}"
[ ${#widened[@]} -eq 0 ] && [ ${#rejected[@]} -eq 0 ] && echo "bounds are up to date"
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
