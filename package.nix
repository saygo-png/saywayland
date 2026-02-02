{niceHaskell, ...}:
niceHaskell.mkPackage {
  flags = niceHaskell.mkFlags {doCheck = false;};
  packageRoot = ./.;
  cabalName = "saywayland";
  compiler = "ghc912";
}
