(require 'org)
(org-babel-tangle-file "./readme.org" "./readme.el")
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(byte-compile-file "./init.el")
