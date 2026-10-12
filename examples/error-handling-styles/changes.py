import os, shutil, subprocess, re, sys, json
EX = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.abspath(EX + '/../..')
BASE = EX + '/base'
SRC = REPO + '/src'
FIELDS = ['wById','wByEmailDefault','wByEmail','wWrite','wRead','wInsert','wCache','wPrefs','wToken','wQuota','wNormA','wNormB']

def sub(t, old, new, n=None):
    c = t.count(old)
    assert c >= 1 and (n is None or c == n), (old[:60], c)
    return t.replace(old, new)

def toold(overrides):
    lines = []
    for f in FIELDS:
        lines.append(f"OT.{f} = {overrides.get(f, f + ' w')}")
    return "toOld :: World -> OT.World\ntoOld w = OT.World\n  { " + "\n  , ".join(lines) + "\n  }"

# ---------------------------------------------------------------- the changes
CHANGES = []
def change(id, kind, title, **kw):
    CHANGES.append(dict(id=id, kind=kind, title=title, **kw))

# ---- S0: no change. Run this first: every property must be ok, otherwise the harness is broken.
change('S0', 'shape', 'baseline: no change (harness check)',
  types=lambda t: t, old={}, edits={'Rail': lambda t: t, 'Case': lambda t: t, 'Helper': lambda t: t})

# ---- S1: Maybe -> Either (a reason for the miss)
change('S1', 'shape', 'cache/prefs lookups: Maybe a  ->  Either CacheError a',
  types=lambda t: sub(sub(sub(sub(sub(t,
      "  , cacheLookup  :: UserId -> Eff es (Maybe User)", "  , cacheLookup  :: UserId -> Eff es (Either CacheError User)"),
      "  , prefsOf      :: UserId -> Eff es (Maybe Prefs)", "  , prefsOf      :: UserId -> Eff es (Either CacheError Prefs)"),
      "  , wCache :: Maybe User, wPrefs :: Maybe Prefs", "  , wCache :: Either CacheError User, wPrefs :: Either CacheError Prefs"),
      "import Test.QuickCheck hiding (Success, Failure)\n",
      "import Test.QuickCheck hiding (Success, Failure)\n\ndata CacheError = CacheMiss | CacheDown deriving (Eq, Show)\ninstance Arbitrary CacheError where arbitrary = elements [CacheMiss, CacheDown]\n"), "validateName ::", "validateName ::"),
  old={'wCache': 'either (const Nothing) Just (wCache w)', 'wPrefs': 'either (const Nothing) Just (wPrefs w)'},
  edits={
    'Rail': lambda t: t,
    'Case': lambda t: sub(sub(t,
        "  case cached of\n    Just u  -> pure u\n    Nothing -> do", "  case cached of\n    Right u -> pure u\n    Left _  -> do"),
        "  pure $ case m of\n    Just p  -> p\n    Nothing -> defaultPrefs", "  pure $ case m of\n    Right p -> p\n    Left _  -> defaultPrefs"),
    'Helper': lambda t: sub(sub(t,
        "  case cached of\n    Just u  -> pure u\n    Nothing -> do", "  case cached of\n    Right u -> pure u\n    Left _  -> do"),
        "fromMaybe defaultPrefs <$> prefsOf db i", "either (const defaultPrefs) id <$> prefsOf db i"),
  })

# ---- S2: Maybe -> [Maybe] (sharded cache; first hit wins)
change('S2', 'shape', 'cache/prefs lookups: Maybe a  ->  [Maybe a]  (shards; first hit wins)',
  types=lambda t: sub(sub(sub(t,
      "  , cacheLookup  :: UserId -> Eff es (Maybe User)", "  , cacheLookup  :: UserId -> Eff es [Maybe User]"),
      "  , prefsOf      :: UserId -> Eff es (Maybe Prefs)", "  , prefsOf      :: UserId -> Eff es [Maybe Prefs]"),
      "  , wCache :: Maybe User, wPrefs :: Maybe Prefs", "  , wCache :: [Maybe User], wPrefs :: [Maybe Prefs]"),
  old={'wCache': 'listToMaybe (catMaybes (wCache w))', 'wPrefs': 'listToMaybe (catMaybes (wPrefs w))'},
  edits={
    'Rail': lambda t: sub(sub(sub(t,
        "  cacheLookup db i\n    ?| one", "  cacheLookup db i <&> listToMaybe . successes\n    ?| one"),
        "prefsFor db i = prefsOf db i ?~ defaultPrefs", "prefsFor db i = (prefsOf db i <&> listToMaybe . successes) ?~ defaultPrefs"),
        "import Data.Functor ((<&>))\n", "import Data.Functor ((<&>))\nimport Data.Maybe (listToMaybe)\n"),
    'Case': lambda t: sub(sub(sub(t,
        "  case cached of\n", "  case listToMaybe (catMaybes cached) of\n"),
        "  pure $ case m of\n", "  pure $ case listToMaybe (catMaybes m) of\n"),
        "import Data.Foldable (for_)\n", "import Data.Foldable (for_)\nimport Data.Maybe (catMaybes, listToMaybe)\n"),
    'Helper': lambda t: sub(sub(sub(t,
        "  case cached of\n", "  case listToMaybe (catMaybes cached) of\n"),
        "fromMaybe defaultPrefs <$> prefsOf db i", "fromMaybe defaultPrefs . listToMaybe . catMaybes <$> prefsOf db i"),
        "import Data.Maybe (catMaybes, fromMaybe)", "import Data.Maybe (catMaybes, fromMaybe, listToMaybe)"),
  })

# ---- S3: [User] -> Maybe User (lookup by primary key)
change('S3', 'shape', 'usersById: Either DbError [User]  ->  Either DbError (Maybe User)  (primary key)',
  types=lambda t: sub(sub(sub(sub(t,
      "  { usersById    :: UserId -> Eff es (Either DbError [User])", "  { usersById    :: UserId -> Eff es (Either DbError (Maybe User))"),
      "  { wById :: Either DbError [User], wByEmailDefault", "  { wById :: Either DbError (Maybe User), wByEmailDefault"),
      "    <$> rows <*> rows <*>", "    <$> mrow <*> rows <*>"),
      "      prob  =", "      mrow  = frequency [(1, Left <$> arbitrary), (7, Right <$> arbitrary)]\n      prob  ="),
  old={'wById': 'fmap (maybe [] pure) (wById w)'},
  edits={
    'Rail': lambda t: sub(t,
        "one :: Error Err :> es => Eff es (Either e [a]) -> Eff es (Either (CardinalityError [a]) a)",
        "one :: (Error Err :> es, Foldable t) => Eff es (Either e (t a)) -> Eff es (Either (CardinalityError (t a)) a)"),
    'Case': lambda t: sub(sub(sub(t,
        "  case rows of\n    []  -> throwError NotFound\n    [u] -> pure u\n    _   -> throwError Internal",
        "  case rows of\n    Nothing -> throwError NotFound\n    Just u  -> pure u"),
        "      case byId of\n        [u] -> pure u\n        _   -> do", "      case byId of\n        Just u  -> pure u\n        Nothing -> do"),
        "  pure $ case rows of\n    []  -> \"(unknown)\"\n    [u] -> uname u\n    _   -> \"(ambiguous)\"",
        "  pure $ case rows of\n    Nothing -> \"(unknown)\"\n    Just u  -> uname u"),
    'Helper': lambda t: sub(sub(sub(sub(t,
        "expectOne :: Error Err :> es => Err -> Err -> [a] -> Eff es a\nexpectOne onNone onMany = \\case\n",
        "expectOne :: (Error Err :> es, Foldable t) => Err -> Err -> t a -> Eff es a\nexpectOne onNone onMany xs = case toList xs of\n"),
        "single :: [a] -> Maybe a\nsingle [x] = Just x\nsingle _   = Nothing",
        "single :: Foldable t => t a -> Maybe a\nsingle xs = case toList xs of\n  [x] -> Just x\n  _   -> Nothing"),
        "import Data.Foldable (for_)", "import Data.Foldable (for_, toList)"),
        "  pure $ case rows of\n    []  -> \"(unknown)\"\n    [u] -> uname u\n    _   -> \"(ambiguous)\"",
        "  pure $ case rows of\n    Nothing -> \"(unknown)\"\n    Just u  -> uname u"),
  })

# ---- S4: [Bool] -> Bool (the query became SELECT EXISTS)
change('S4', 'shape', 'permitted: Either DbError [Bool]  ->  Either DbError Bool  (SELECT EXISTS)',
  types=lambda t: re.sub(r"      bools = [^\n]*\n", "", sub(sub(sub(sub(t,
      "(Either DbError [Bool])\n  , insertGrant", "(Either DbError Bool)\n  , insertGrant"),
      "  , wWrite :: Either DbError [Bool], wRead :: Either DbError [Bool]", "  , wWrite :: Either DbError Bool, wRead :: Either DbError Bool"),
      "    <*> bools <*> bools <*> arbitrary", "    <*> boolE <*> boolE <*> arbitrary"),
      "      prob  =", "      boolE = frequency [(1, Left <$> arbitrary), (8, pure (Right True)), (4, pure (Right False))]\n      prob  =")),
  old={'wWrite': 'fmap pure (wWrite w)', 'wRead': 'fmap pure (wRead w)'},
  edits={
    'Rail': lambda t: sub(t, "?! cardinalityErr Internal (const Internal) ", "", 3),
    'Case': lambda t: sub(sub(sub(t,
        "  rows <- case r of\n    Left _  -> throwError Internal\n    Right x -> pure x\n  ok <- case rows of\n    [b] -> pure b\n    _   -> throwError Internal\n",
        "  ok <- case r of\n    Left _  -> throwError Internal\n    Right x -> pure x\n"),
        "  wrows <- case rw of\n    Left _  -> audit db \"permission query failed\" >> throwError Internal\n    Right x -> pure x\n  mayWrite <- case wrows of\n    [b] -> pure b\n    _   -> throwError Internal\n",
        "  mayWrite <- case rw of\n    Left _  -> audit db \"permission query failed\" >> throwError Internal\n    Right x -> pure x\n"),
        "  rrows <- case rr of\n    Left _  -> throwError Internal\n    Right x -> pure x\n  already <- case rrows of\n    [b] -> pure b\n    _   -> throwError Internal\n",
        "  already <- case rr of\n    Left _  -> throwError Internal\n    Right x -> pure x\n"),
    'Helper': lambda t: sub(t, " >>= expectOne Internal Internal", "", 3),
  })

# ---- S5: Validation -> Either (validateName refactored to share with another module)
change('S5', 'shape', 'validateName: Validation [String] a  ->  Either [String] a  (refactor)',
  types=lambda t: re.sub(r"validateName :: .*?  where check ok msg = [^\n]*\n",
      lambda m: ('validateName :: String -> Either [String] String\n'
                 'validateName n = case [msg | (False, msg) <- checks] of\n'
                 '    [] -> Right n\n'
                 '    ps -> Left ps\n'
                 '  where checks = [ (not (null n), "empty name: " ++ show n)\n'
                 '                 , (length n <= 10, "too long: " ++ n)\n'
                 '                 , (\' \' `notElem` n, "has space: " ++ n) ]\n'), t, flags=re.S),
  old={},
  edits={
    'Rail': lambda t: sub(t, "pure (map validateName names) ?? BadRequest", "pure (map (fromEither . validateName) names) ?? BadRequest"),
    'Case': lambda t: sub(t, "case traverse validateName names of", "case traverse (fromEither . validateName) names of"),
    'Helper': lambda t: sub(t, "case traverse validateName names of", "case traverse (fromEither . validateName) names of"),
  })

# ---- P1: the error depends on the kind of database failure
def p1_core(t):
    t = sub(t, "  deriving (Eq, Show)\ndata DbError = DbError deriving (Eq, Show)",
               "  deriving (Eq, Show)\ndata DbError = Timeout | Deadlock | Other deriving (Eq, Show)")
    t = sub(t, "Unavailable | TooManyRequests", "Unavailable | GatewayTimeout | TooManyRequests")
    t = sub(t, "instance Arbitrary DbError where arbitrary = pure DbError", "instance Arbitrary DbError where arbitrary = elements [Timeout, Deadlock, Other]")
    return t + "\ndbErr :: DbError -> Err\ndbErr Timeout = GatewayTimeout\ndbErr _       = Unavailable\n"
P1_UNIT = '''
okW :: World
okW = World { wById = Right [u1], wByEmailDefault = Right [], wByEmail = [], wWrite = Right [True], wRead = Right [False]
            , wInsert = Right (), wCache = Nothing, wPrefs = Nothing, wToken = Right "tok", wQuota = Right 1
            , wNormA = Right "a@x", wNormB = Right "b@x" }
  where u1 = User 1 "a@x" "ann"
units :: [(String, Handlers -> Bool)]
units =
  [ ("register timeout is 504", \\h -> run (hRegister h (mkDb okW { wByEmailDefault = Left Timeout }) "a@x") == (Left GatewayTimeout, []))
  , ("register deadlock is 503", \\h -> run (hRegister h (mkDb okW { wByEmailDefault = Left Deadlock }) "a@x") == (Left Unavailable, []))
  , ("getUser timeout is 504", \\h -> run (hGetUser h (mkDb okW { wById = Left Timeout }) 1) == (Left GatewayTimeout, []))
  , ("quota timeout is 504", \\h -> run (hUseQuota h (mkDb okW { wQuota = Left Timeout }) 1) == (Left GatewayTimeout, []))
  , ("permission timeout stays 500", \\h -> run (hCheckWrite h (mkDb okW { wWrite = Left Timeout }) 1 1) == (Left Internal, []))
  ]
'''
change('P1', 'policy', 'a database timeout becomes 504, other database failures stay 503',
  core=p1_core, unit=P1_UNIT,
  edits={
    'Rail': lambda t: sub(sub(t, "? Unavailable", "?? dbErr"), "Eff es (Either e [a])", "Eff es (Either DbError [a])"),
    'Case': lambda t: sub(t, "Left _  -> throwError Unavailable", "Left e  -> throwError (dbErr e)"),
    'Helper': lambda t: sub(sub(t, "orThrow Unavailable", "orDb"),
        "expectOne ::", "orDb :: Error Err :> es => Eff es (Either DbError a) -> Eff es a\norDb q = q >>= either (throwError . dbErr) pure\n\nexpectOne ::"),
  })

# ---- P2: several grants per user; any granted row suffices
P2_UNIT = P1_UNIT.split('units ::')[0] + '''units :: [(String, Handlers -> Bool)]
units =
  [ ("any True row grants write", \\h -> run (hCheckWrite h (mkDb okW { wWrite = Right [False, True] }) 1 1) == (Right (), []))
  , ("all False is forbidden", \\h -> run (hCheckWrite h (mkDb okW { wWrite = Right [False, False] }) 1 1) == (Left Forbidden, []))
  , ("no rows is forbidden", \\h -> run (hCheckWrite h (mkDb okW { wWrite = Right [] }) 1 1) == (Left Forbidden, []))
  , ("db error is still 500", \\h -> run (hCheckWrite h (mkDb okW { wWrite = Left DbError }) 1 1) == (Left Internal, []))
  , ("existing read grant among several conflicts", \\h -> run (hGrantRead h (mkDb okW { wByEmailDefault = Right [User 2 "b@x" "bo"], wRead = Right [False, True] }) 1 1 "b@x") == (Left Conflict, []))
  ]
'''
change('P2', 'policy', 'users can hold several grants: any granting row suffices (was: exactly one row)',
  unit=P2_UNIT,
  edits={
    'Rail': lambda t: sub(t, "?! cardinalityErr Internal (const Internal)", "<&> or", 3),
    'Case': lambda t: sub(sub(sub(t,
        "  ok <- case rows of\n    [b] -> pure b\n    _   -> throwError Internal\n", "  let ok = or rows\n"),
        "  mayWrite <- case wrows of\n    [b] -> pure b\n    _   -> throwError Internal\n", "  let mayWrite = or wrows\n"),
        "  already <- case rrows of\n    [b] -> pure b\n    _   -> throwError Internal\n", "  let already = or rrows\n"),
    'Helper': lambda t: sub(sub(t, " >>= expectOne Internal Internal", " <&> or", 3),
        "import Data.Either (isLeft)\n", "import Data.Either (isLeft)\nimport Data.Functor ((<&>))\n"),
  })

# ---- P3: log duplicate registrations
P3_UNIT = P1_UNIT.split('units ::')[0] + '''units :: [(String, Handlers -> Bool)]
units =
  [ ("duplicate is logged and conflicts", \\h -> run (hRegister h (mkDb okW { wByEmailDefault = Right [User 1 "a@x" "ann"] }) "a@x") == (Left Conflict, ["duplicate registration"]))
  , ("fresh email logs nothing", \\h -> run (hRegister h (mkDb okW) "a@x") == (Right "tok", []))
  , ("db failure is not a duplicate", \\h -> run (hRegister h (mkDb okW { wByEmailDefault = Left DbError }) "a@x") == (Left Unavailable, []))
  ]
'''
change('P3', 'policy', 'audit-log every duplicate registration attempt',
  unit=P3_UNIT,
  edits={
    'Rail': lambda t: sub(t, "usersByEmail db e ? Unavailable ?∅ const Conflict",
        "usersByEmail db e ? Unavailable <&> noneAtAll ?<* (\\_ -> audit db \"duplicate registration\") ?? const Conflict"),
    'Case': lambda t: sub(t, "    _  -> throwError Conflict", "    _  -> audit db \"duplicate registration\" >> throwError Conflict"),
    'Helper': lambda t: sub(t, "unless (null rows) (throwError Conflict)", "unless (null rows) (audit db \"duplicate registration\" >> throwError Conflict)"),
  })

# ---- P4: a third, legacy source in the fallback chain
def p4_types(t):
    t = sub(t, "  , audit        :: String -> Eff es ()\n  }", "  , legacyByEmail :: Email -> Eff es (Either DbError [User])\n  , audit        :: String -> Eff es ()\n  }")
    t = sub(t, "wNormB :: Either [String] Email\n  } deriving Show", "wNormB :: Either [String] Email\n  , wLegacy :: Either DbError [User]\n  } deriving Show")
    t = sub(t, "  , audit        = \\s -> modify (++ [s])", "  , legacyByEmail = \\_ -> pure (wLegacy w)\n  , audit        = \\s -> modify (++ [s])")
    t = sub(t, "    <*> prob <*> prob\n", "    <*> prob <*> prob <*> rows\n")
    return t
P4_UNIT = '''
okW :: World
okW = World { wById = Right [], wByEmailDefault = Right [], wByEmail = [], wWrite = Right [True], wRead = Right [False]
            , wInsert = Right (), wCache = Nothing, wPrefs = Nothing, wToken = Right "tok", wQuota = Right 1
            , wNormA = Right "a@x", wNormB = Right "b@x", wLegacy = Right [] }
u1 :: User
u1 = User 1 "a@x" "ann"
units :: [(String, Handlers -> Bool)]
units =
  [ ("legacy source answers", \\h -> run (hFindUser h (mkDb okW { wLegacy = Right [u1] }) 1 "a@x") == (Right u1, []))
  , ("nothing anywhere is 404", \\h -> run (hFindUser h (mkDb okW) 1 "a@x") == (Left NotFound, []))
  , ("legacy failure is 503", \\h -> run (hFindUser h (mkDb okW { wLegacy = Left DbError }) 1 "a@x") == (Left Unavailable, []))
  , ("legacy ambiguity is 500", \\h -> run (hFindUser h (mkDb okW { wLegacy = Right [u1, u1] }) 1 "a@x") == (Left Internal, []))
  , ("email source wins over legacy", \\h -> run (hFindUser h (mkDb okW { wByEmailDefault = Right [u1], wLegacy = Left DbError }) 1 "a@x") == (Right u1, []))
  ]
'''
change('P4', 'policy', 'add a third, legacy source to the user lookup fallback chain',
  types=p4_types, unit=P4_UNIT.replace('okW :: World', 'okW :: World', 1),
  edits={
    'Rail': lambda t: sub(t, "    ?| one (usersByEmail db e)\n", "    ?| one (usersByEmail db e)\n    ?| one (legacyByEmail db e)\n"),
    'Case': lambda t: sub(t,
        "          case byEmail of\n            []  -> throwError NotFound\n            [u] -> pure u\n            _   -> throwError Internal\n",
        "          case byEmail of\n            [u] -> pure u\n            _   -> do\n              r3 <- legacyByEmail db e\n              legacy <- case r3 of\n                Left _  -> throwError Unavailable\n                Right x -> pure x\n              case legacy of\n                []  -> throwError NotFound\n                [u] -> pure u\n                _   -> throwError Internal\n"),
    'Helper': lambda t: sub(t,
        "        Nothing -> orThrow Unavailable (usersByEmail db e) >>= expectOne NotFound Internal\n",
        "        Nothing -> do\n          byEmail <- single <$> orThrow Unavailable (usersByEmail db e)\n          case byEmail of\n            Just u  -> pure u\n            Nothing -> orThrow Unavailable (legacyByEmail db e) >>= expectOne NotFound Internal\n"),
  })

# ---------------------------------------------------------------- machinery
NIX = ['nix-shell', '--run']
def sh(cmd, cwd=None, timeout=900):
    # NO_NIX=1 runs the commands in the current environment instead of `nix-shell` (needs ghc with
    # effectful, validation, QuickCheck and mtl on the package path).
    argv = ['bash', '-c', cmd] if os.environ.get('NO_NIX') else NIX + [cmd]
    r = subprocess.run(argv, cwd=REPO, capture_output=True, text=True, timeout=timeout)
    return r.stdout + r.stderr

def ensure_old():
    # Old.Types / Old.Case: copies of the base Types and Case in their own namespace. They are the
    # reference implementation that shape changes are compared against.
    d = BASE + '/Old'
    os.makedirs(d, exist_ok=True)
    ty = open(BASE + '/Types.hs').read().replace('module Types (module Core, module Types)', 'module Old.Types (module Core, module Old.Types)')
    ca = open(BASE + '/Case.hs').read().replace('module Case', 'module Old.Case').replace('import Types', 'import Old.Types')
    open(d + '/Types.hs', 'w').write(ty)
    open(d + '/Case.hs', 'w').write(ca)

def rw(p, f):
    s = open(p).read()
    s = f(s)
    open(p, 'w').write(s)

def build_dir(path, c, fixed):
    ensure_old()
    if os.path.exists(path): shutil.rmtree(path)
    shutil.copytree(BASE, path, ignore=shutil.ignore_patterns('o', 'o2', 'main', 'main2', '*.tmpl'))
    # data layer
    if 'core' in c:
        rw(path + '/Core.hs', c['core'])
    if 'types' in c:
        rw(path + '/Types.hs', c['types'])
    # oracle
    if c['kind'] == 'shape':
        t = open(BASE + '/OracleOldCase.hs.tmpl').read().replace('@@TOOLD@@', toold(c['old']))
    else:
        t = open(BASE + '/OracleOwnCase.hs.tmpl').read()
    open(path + '/Oracle.hs', 'w').write(t)
    if 'unit' in c and fixed:
        open(path + '/Unit.hs', 'w').write('{-# LANGUAGE GHC2021 #-}\nmodule Unit where\nimport H\nimport Types\n' + c['unit'])
    if fixed:
        for st in ('Rail', 'Case', 'Helper'):
            rw(f'{path}/{st}.hs', c['edits'][st])

def diffstat(a, b):
    out = subprocess.run(['diff', '-U0', a, b], capture_output=True, text=True).stdout
    hunks = len(re.findall(r'^@@', out, re.M))
    plus = len([l for l in out.splitlines() if l.startswith('+') and not l.startswith('+++')])
    minus = len([l for l in out.splitlines() if l.startswith('-') and not l.startswith('---')])
    return hunks, plus, minus

def top_levels(path):
    lines = open(path).read().split('\n'); marks = []
    for i, l in enumerate(lines, 1):
        m = re.match(r'^([a-z][A-Za-z0-9_\']*) ::', l)
        if m: marks.append((i, m.group(1)))
    return marks

def attribute(path, errlines):
    marks = top_levels(path); names = set()
    for ln in errlines:
        cand = [n for (i, n) in marks if i <= ln]
        if cand: names.add(cand[-1])
    return names

def compile_style(dirp, st):
    out = sh(f"cd {dirp} && ghc -fno-code -i{SRC} -i. {st}.hs 2>&1")
    errs = re.findall(rf'{st}\.hs:(\d+):\d+: error', out)
    return sorted(set(attribute(f'{dirp}/{st}.hs', [int(x) for x in errs]))), len(errs), out

def run_main(dirp, defer):
    flags = '-fdefer-type-errors -Wno-deferred-type-errors' if defer else ''
    out = sh(f"cd {dirp} && ghc -O0 {flags} -i{SRC} -i. -outputdir o -o main Main.hs -package QuickCheck 2>&1 | grep -E 'rror' -A6 | head -20; ./main")
    return out

def main(ids):
    report = {}
    for c in CHANGES:
        if ids and c['id'] not in ids: continue
        r = {'title': c['title'], 'kind': c['kind']}
        naive = f"{EX}/var/{c['id']}/naive"; fixed = f"{EX}/var/{c['id']}/fixed"
        build_dir(fixed, c, True)
        if c['kind'] == 'shape':
            build_dir(naive, c, False)
            r['naive'] = {}
            for st in ('Rail', 'Case', 'Helper'):
                names, n, _ = compile_style(naive, st)
                r['naive'][st] = {'loud_fns': names, 'errors': n}
            out = run_main(naive, True)
            st_ = {}
            for line in out.splitlines():
                p = line.split()
                if len(p) == 3 and p[0] in ('rail', 'case', 'helper'): st_[(p[0], p[1])] = p[2]
            r['naive_status'] = {f'{k[0]} {k[1]}': v for k, v in st_.items()}
        out = run_main(fixed, False)
        r['fixed_out'] = [l for l in out.splitlines() if l.strip()]
        r['edit'] = {}
        for st in ('Rail', 'Case', 'Helper'):
            r['edit'][st] = diffstat(f'{BASE}/{st}.hs', f'{fixed}/{st}.hs')
        report[c['id']] = r
        json.dump(report, open(f'{EX}/report_{"_".join(ids) if ids else "all"}.json', 'w'), indent=1)
        print('done', c['id'], flush=True)

if __name__ == '__main__':
    main(sys.argv[1:])
