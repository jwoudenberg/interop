let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  ruby = pkgs.callPackage ./test/ruby-tests/default.nix { };
  gems = pkgs.bundlerEnv {
    name = "ruby-tests";
    ruby = pkgs.ruby;
    gemdir = ./test/ruby-tests;
  };

in pkgs.haskellPackages.shellFor {
  packages = p: [ (import ./default.nix) ];
  buildInputs = [
    # Haskell
    pkgs.cabal-install
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hpack
    pkgs.ormolu

    # Ruby
    gems
    gems.wrappedRuby
    pkgs.bundix
  ];
}
