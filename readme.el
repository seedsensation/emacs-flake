;; -*- lexical-binding: t; -*-

(setq display-line-numbers 'relative
      ring-bell-function 'ignore
      inhibit-startup-screen t

      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t
      completion-styles '(basic substring partial-completion flex))
      warning-suppress-log-types '((files missing-lexbind-cookie))

(setq emacs-flake-dir "~/projects/emacs-flake")

(setq custom-file "~/.emacs.d/custom.el")

(let ((dir (if emacs-flake-dir emacs-flake-dir "./")))
  (if (or
       (file-newer-than-file-p (expand-file-name "custom.el" dir)
      			       "~/.emacs.d/custom.el")
       (not (file-exists-p "~/.emacs.d/custom.el")))
      (copy-file (expand-file-name "emacs-customizations.el" dir) "~/.emacs.d/custom.el" t)
    (when (not (file-exists-p "~/.emacs.d/custom.el"))
      (copy-file "~/.emacs.d/custom.el" (expand-file-name "custom.el" dir) t))))
(load "~/.emacs.d/custom.el")

(load-theme 'gruvbox t)

(set-face-attribute 'default nil :font "Maple Mono" :height 160)

(toggle-truncate-lines 1)

(require 'evil)
(require 'electric)

(evil-mode 1)
(vertico-mode 1)
(ivy-mode 1)
(ivy-prescient-mode 1)
(org-roam-db-autosync-mode 1)
(which-key-mode 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(indent-tabs-mode 0)

(evil-set-undo-system 'undo-redo)

(setq org-roam-directory (file-truename "~/org")
      org-id-locations-file (expand-file-name ".org-id-locations" org-roam-directory)
      org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))

(setq org-preview-latex-default-process 'dvipng)

(defun org-mode-enable ()
  (org-fragtog-mode 1)
  (org-indent-mode 1)
  (org-roam-db-autosync-mode)
  )
(add-hook 'org-mode-hook 'org-mode-enable)

(defun prog-mode-enable ()
  (display-line-numbers-mode 1)
  (format-all-mode)
  (company-mode))
(add-hook 'prog-mode-hook 'prog-mode-enable)

;; input
;; (lsp-attach-hook 'java-mode-hook 'python-mode-hook)
;;
;; output
;; (add-hook 'java-mode-hook 'lsp-mode)
;; (add-hook 'python-mode-hook 'lsp-mode)

(defmacro lsp-attach-hook (&rest args)
  `(progn ,@(mapcar #'(lambda (a) `(add-hook ,a 'lsp-mode)) args) t))
(lsp-attach-hook 'java-mode-hook 'lsp-mode-hook)

(add-hook 'text-mode-hook 'visual-line-mode)

(defun org-cycle-hide-drawers (state)
  "Re-hide all drawers after a visibility state change."
  (when (and (derived-mode-p 'org-mode)
	     (not (memq state '(overview folded contents))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
	     (beg (if globalp
		      (point-min)
		    (point)))
	     (end (if globalp
		      (point-max)
		    (if (eq state 'children)
			(save-excursion
			  (outline-next-heading)
			  (point))
		      (org-end-of-subtree t)))))
	(goto-char beg)
	(while (re-search-forward org-drawer-regexp end t)
	  (save-excursion
	    (beginning-of-line 1)
	    (when (looking-at org-drawer-regexp)
	      (let* ((start (1- (match-beginning 0)))
		     (limit
		      (save-excursion
			(outline-next-heading)
			(point)))
		     (msg (format
			   (concat
			    "org-cycle-hide-drawers:  "
			    "`:END:`"
			    " line missing at position %s")
			   (1+ start))))
		(if (re-search-forward "^[ \t]*:END:" limit t)
		    (outline-flag-region start (line-end-position) t)
		                    (user-error msg))))))))))

(defun +evil/window-split-and-follow()
  "Split current window horizontally, then focus on new window.
   If `evil-split-window-below` is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-split-window-below (not evil-split-window-below)))
    (call-interactively #'evil-window-split)))

(defun +evil/window-vsplit-and-follow()
  "Split current window vertically, then focus on new window.
   If `evil-split-window-below` is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
    (call-interactively #'evil-window-vsplit)))

(defun org-id-reload-all ()
    (interactive)
    (org-id-update-id-locations)
    (org-roam-update-org-id-locations)
    (org-roam-db-sync))

(evil-define-key 'normal org-mode-map (kbd "<TAB>") 'org-cycle)

(evil-define-key '(list normal motion visual) global-map (kbd "-") 'avy-goto-char)
(evil-define-key '(list normal motion visual) global-map (kbd "_") 'avy-goto-line)

(evil-define-key 'normal global-map (kbd "=") 'pop-global-mark)

(keymap-global-set "M-o" 'ace-window)

(keymap-global-set "C-x C-<up>" 'electric-buffer-list)
(keymap-global-set "C-x C-b" 'electric-buffer-list)

(setq electric-buffer-menu-mode-map (make-keymap))
(define-key electric-buffer-menu-mode-map (kbd "o") 'Electric-buffer-menu-select)



(evil-define-key 'visual global-map "S" 'surround-insert)

(defvar window-map (define-keymap 
		     "h" #'evil-window-left
		     "j" #'evil-window-down
		     "k" #'evil-window-up
		     "l" #'evil-window-right
                     "H" #'window-move-left
		     "J" #'window-move-down
		     "K" #'window-move-up
		     "L" #'window-move-right
		     "v" #'+evil/window-vsplit-and-follow
		     "n" #'+evil/window-split-and-follow
		     "r" #'redraw-display
		     ))

(defvar config-map (define-keymap 
		     "r" (lambda () (interactive) (load "~/nixos/emacs/init.el"))
		     "h" (lambda () (interactive) (find-file "~/org/contents.org"))
		     "c" (lambda () (interactive) (find-file "~/projects/emacs-flake/readme.org"))
		     "k" (lambda () (interactive) (find-file "~/projects/emacs-flake/readme.org"))
		     "d" (lambda () (interactive) (dired "./"))
		     ))

(defvar view-map (define-keymap 
		   "v" #'org-toggle-narrow-to-subtree
		   "h" (lambda() (interactive) (org-cycle-hide-drawers 'all))
		   "l" #'lsp-describe-at-point
		   ))

(defvar buffer-map (define-keymap :full t
		     "p" #'previous-buffer
		     "n" #'next-buffer
		     "b" #'buffer-menu
		     ))

(defvar customize-map (define-keymap
		        "c" #'customize-browse
			"f" #'list-faces-display
			"v" #'customize-variable
			"g" #'customize-group
			))

(defvar project-map (define-keymap
		      "t" #'treemacs
		      "e" (lambda () (interactive) (lsp-treemacs-errors-list))
		      "x" #'projectile-compile-project
		      "p" (lambda () (interactive)
			    (projectile-switch-project))
		      "l" #'lsp
		      "v" #'vterm
		      "s" (lambda () (interactive)
			    (lsp-treemacs-errors-list)
			    (treemacs)
			    (lsp))
		      ))

(defvar leader-map (define-keymap
  "." #'find-file
  "w" window-map
  "d" config-map
  "v" view-map
  "b" buffer-map
  "c" customize-map
  "h" help-map
  "/" #'avy-goto-char-2
  ))

(defmacro set-local-leader-map (source-map key &rest args)
  `(progn
     (evil-define-key 'normal ,source-map (kbd "<SPC>")
       (let ((map (make-sparse-keymap)))
	 (set-keymap-parent map leader-map)
	 (define-key map (kbd ,key) ,(append
				     (list 'define-keymap)
				     args))

	 map))))

(set-local-leader-map org-mode-map "m"
			"." #'consult-org-heading
			"l i" #'org-id-get-create
			"i" #'org-roam-node-insert
			"f" #'org-roam-node-find
			"r" #'org-id-reload-all
			"b" #'org-mark-ring-goto
			)

(evil-define-key 'normal global-map (kbd "<SPC>") leader-map)
