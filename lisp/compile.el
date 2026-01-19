(require 'org)
(org-babel-tangle-file "./readme.org" "./readme.el")
(byte-compile-file "./readme.el")
(byte-compile-file "./init.el")
