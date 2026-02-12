;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(format-all-default-formatters
   '(("Assembly" asmfmt) ("ATS" atsfmt) ("Bazel" buildifier)
     ("BibTeX" emacs-bibtex) ("C" clang-format) ("C#" csharpier)
     ("C++" clang-format) ("Cabal Config" cabal-fmt)
     ("Clojure" zprint) ("CMake" cmake-format) ("Crystal" crystal)
     ("CSS" prettier) ("Cuda" clang-format) ("D" dfmt)
     ("Dart" dart-format) ("Dhall" dhall) ("Dockerfile" dockfmt)
     ("Elixir" mix-format) ("Elm" elm-format)
     ("Emacs Lisp" emacs-lisp) ("Erlang" efmt) ("F#" fantomas)
     ("Fish" fish-indent) ("Fortran Free Form" fprettify)
     ("GLSL" clang-format) ("Go" gofmt) ("GraphQL" prettier)
     ("Haskell" brittany) ("HCL" hclfmt) ("HLSL" clang-format)
     ("HTML" html-tidy) ("HTML+EEX" mix-format)
     ("HTML+ERB" erb-format) ("Hy" emacs-hy) ("Java" astyle)
     ("JavaScript" prettier) ("JSON" prettier) ("JSON5" prettier)
     ("Jsonnet" jsonnetfmt) ("JSX" prettier) ("Kotlin" ktlint)
     ("LaTeX" latexindent) ("Less" prettier)
     ("Literate Haskell" brittany) ("Lua" lua-fmt)
     ("Markdown" prettier) ("Meson" muon-fmt) ("Nix" nixpkgs-fmt)
     ("Objective-C" clang-format) ("OCaml" ocp-indent)
     ("Perl" perltidy) ("PHP" prettier)
     ("Protocol Buffer" clang-format) ("PureScript" purty)
     ("Python" black) ("R" styler) ("Reason" bsrefmt)
     ("ReScript" rescript) ("Ruby" rufo) ("Rust" rustfmt)
     ("Scala" scalafmt) ("SCSS" prettier) ("Shell" shfmt)
     ("Solidity" prettier) ("SQL" sqlformat) ("Svelte" prettier)
     ("Swift" swiftformat) ("Terraform" terraform-fmt)
     ("TOML" prettier) ("TSX" prettier) ("TypeScript" prettier)
     ("V" v-fmt) ("Verilog" istyle-verilog) ("Vue" prettier)
     ("XML" html-tidy) ("YAML" prettier) ("Zig" zig)
     ("_Angular" prettier) ("_AZSL" clang-format)
     ("_Beancount" bean-format) ("_Caddyfile" caddy-fmt)
     ("_Flow" prettier) ("_Gleam" gleam) ("_Ledger" ledger-mode)
     ("_Nginx" nginxfmt) ("_Snakemake" snakefmt)))
 '(format-all-formatters '(("Java" astyle)) t)
 '(initial-buffer-choice "~/org/contents.org")
 '(lsp-java-vmargs
   '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4"
     "-XX:AdaptiveSizePolicyWeight=90"
     "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms100m"))
 '(org-enforce-todo-dependencies t)
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.25 :html-foreground
		 "Black" :html-background "Transparent" :html-scale
		 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-roam-directory "/home/mercury/org/")
 '(org-roam-extract-new-file-path "nodes/${slug}.org")
 '(recentf-mode t)
 '(safe-local-variable-values '((lexical-bindings . t)))
 '(truncate-lines t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#282828" :foreground "#ebdbb2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 158 :width normal :foundry "    " :family "Maple Mono"))))
 '(ivy-current-match ((t (:extend t :foreground "#ffffc8" :underline nil :slant italic :weight bold))))
 '(org-level-4 ((t (:extend nil :foreground "medium spring green" :weight normal)))))
