{
  description = "Custom Emacs Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
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
  in
  {
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
              make compile
            '';
            installPhase = ''
              pwd
              ls
              mkdir $out
              cp *.el $out
              cp *.elc $out
              cp -r lisp $out/lisp
            '';



          };
      }
    );
  };
}
