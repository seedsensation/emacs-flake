(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that.
  (if (file-exists-p (expand-file-name "readme.elc" user-emacs-directory))
      (load-file (expand-file-name "readme.elc" user-emacs-directory))
    ;; otherwise tangle and load
    (org-babel-load-file (expand-file-name "readme.org" user-emacs-directory))))
