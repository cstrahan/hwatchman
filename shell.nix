with (import <nixpkgs> { });

let
  #ghc = haskellngPackages;
  ghc = haskell-ng.packages.ghc7101;

  exposePkgs = names: ghcPackages: ghcPackages.overrideDerivation (drv: { postBuild = ''
    ${drv.postBuild}
    ${lib.concatMapStringsSep "\n" (name: "$out/bin/ghc-pkg expose ${name}") names}
  ''; });


  withHoogle = haskellEnv:
    ghc.callPackage <nixpkgs/pkgs/development/libraries/haskell/hoogle/local.nix> {
      packages = haskellEnv.paths;
    };

  ghcPackages = exposePkgs [ ] (ghc.ghcWithPackages (p: with p; [
    #ipprint
    #hscolour
    #ghc-mod
    #hdevtools
    stylish-haskell
    cabal-install
    #cabal2nix
    vector
    hspec
    deepseq
    #criterion
    cereal
    binary
    hashmap
  ]));

in

with pkgs;

runCommand "dummy" {
  buildInputs = [
    ghcPackages
    (withHoogle ghcPackages)
    pkgconfig
    pythonPackages.pygments
  ];
  shellHook = ''
    export NIX_GHC="${ghcPackages}/bin/ghc"
    export NIX_GHCPKG="${ghcPackages}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${ghcPackages}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
} ""
