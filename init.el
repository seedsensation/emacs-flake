;; -*- lexical-binding: t; -*-

(setq file-to-open (if (string-equal (getenv "TEST") "1") "readme-live.org" "readme.org"))
(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that.
  (if (file-exists-p (expand-file-name "readme.el" user-emacs-directory))
      (load-file (expand-file-name "readme.el" user-emacs-directory))
    ;; otherwise tangle and load
    (org-babel-load-file (expand-file-name 'file-to-open user-emacs-directory))))
