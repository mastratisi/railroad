{pkgs ? import <nixpkgs> {}
}:
let
  deps = pkgs.haskellPackages.callCabal2nix "railroad" ./. {};
  haskell-language-server-no-hlint = pkgs.haskell.lib.disableCabalFlag pkgs.haskellPackages.haskell-language-server "hlint";
in
pkgs.haskellPackages.shellFor {
  packages = p: [ deps ];
  nativeBuildInputs = with pkgs; [
    cabal-install
    haskell-language-server-no-hlint
  ];
  withHoogle = true;
}
