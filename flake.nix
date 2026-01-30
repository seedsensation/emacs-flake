{
  description = "Custom Emacs Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let
    allSystems = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];

    forAllSystems =
      f: nixpkgs.lib.genAttrs allSystems (
        system:
        f {
          pkgs = import nixpkgs { inherit system; };
        }
      );
      default_pkgs = import nixpkgs { system = "x86_64-linux"; };
  in
  {
    out = let 
    binName = "emacs-flake";
    dependencies = with default_pkgs; [ emacs ];
    in default_pkgs.stdenv.mkDerivation {

            name = "emacs-flake";
            src = self;

            buildInputs = dependencies;
            buildPhase = ''
              make clean compile
            '';
            installPhase = ''
              pwd
              ls
              mkdir $out
              cp *.elc $out
            '';
	    };


    packages = forAllSystems (
      { pkgs }:
      {
        default =
          let
            binName = "emacs-flake";
            dependencies = with pkgs; [
              emacs
            ];
          in
          pkgs.stdenv.mkDerivation {
            name = "emacs-flake";
            src = self;

            buildInputs = dependencies;
            buildPhase = ''
              make clean compile
            '';
            installPhase = ''
              pwd
              ls
              mkdir $out
              cp *.elc $out
	      cp custom.el $out
            '';



          };
      }
    );
  };
}
