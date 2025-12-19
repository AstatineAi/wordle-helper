{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
      ];
      perSystem =
        { pkgs, config, system, ... }:
        let
          hsPkgs = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
            cabal-install
            containers
            parallel
          ]);
        in
        {
          devShells.wasm = pkgs.mkShell {
            packages = [
              inputs.ghc-wasm-meta.packages.${system}.all_9_12
              pkgs.gmp
            ];
          };
          devShells.native = pkgs.mkShell {
            packages = with pkgs; [
              hsPkgs
            ];
          };
          devShells.default = config.devShells.native;
        };
    };
}
