;;; config-editing.el --- Editing and formatting helpers -*- lexical-binding: t; -*-

(use-package markdown-mode :mode "\\.md\\'")

(use-package paredit
  :hook ((emacs-lisp-mode) . paredit-mode))

(use-package apheleia
  :init (apheleia-global-mode 1)
  :config
  ;; apheleia defaults terraform-mode to opentofu
  (setf (alist-get 'terraform-mode apheleia-mode-alist) 'terraform))

(use-package diff-hl
  :init (global-diff-hl-mode)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh)))

;; Use js-json-mode for JSON files (built-in, no auto-formatting)
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-json-mode))

(provide 'config-editing)
