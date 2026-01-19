{
  description = "Emacs Flake";

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
            binName = "Emacs With Packages";
            dependencies = with pkgs; [

              ((pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages ( epkgs:

              (with epkgs.melpaStablePackages; [
                magit
                gruvbox-theme
                org-fragtog
              ]) ++

              (with epkgs.melpaPackages; [
                avy
                company
                consult
                dash
                emacs-everywhere
                evil
                f
                ivy
                ivy-prescient
                magit-section
                marginalia
                nix-mode
                orderless
                simple-httpd
                surround
                vertico
                websocket
              ]) ++

              (with epkgs.elpaPackages; [
                devdocs     
              ]) ++
              (with epkgs; [
                org
                org-roam
                org-roam-ui
                org-roam-timestamps
                sqlite3
                lsp-mode
              ])))


              rust-analyzer
              shellcheck
              tree-sitter
              nil
              tailwindcss-language-server
              nixfmt
              sqlite

              # LaTeX Packages
              (texliveBasic.withPackages (
                ps: with ps; [
                  dvisvgm dvipng
                  wrapfig amsmath
                  ulem hyperref
                  capt-of
                  #(setq org-latex-compiler "lualatex")
                  #(setq org-preview-latex-default-process 'dvisvgm)
                ]))
            ];
          in
          pkgs.stdenv.mkDerivation {
            name = "Emacs With Packages";
            src = self;

            buildInputs = dependencies;
            buildPhase = ''
              make compile
            '';
            installPhase = ''
pwd
ls
mkdir emacs
cp *.el emacs
cp *.elc emacs
cp -r lisp emacs/lisp
            '';



          };
      }
    );
  };
}
