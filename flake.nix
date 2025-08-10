{
  description = "Submarine Commander";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    clj-nix.url = "github:jlesquembre/clj-nix";
  };

  outputs = { self, nixpkgs, flake-utils, clj-nix }:
    flake-utils.lib.eachDefaultSystem (system: {
      packages = let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        default = clj-nix.lib.mkCljApp {
          pkgs = pkgs;
          modules = [
            {
              projectSrc = ./.;
              name = "submarine-commander";
              main-ns = "dev.rob-3.submarine-commander.main";
            }
          ];
        };
        docker = pkgs.dockerTools.buildLayeredImage {
          name = "robs-submarine-commander";
          tag = "latest";
          contents = [default];
          config.Cmd = ["${default}/bin/submarine-commander"];
        };
      };
    });
}
