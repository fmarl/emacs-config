;;; config-ui.el --- Theme, navigation and UI packages -*- lexical-binding: t; -*-

(use-package ef-themes
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("M-<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select))
  :config
  (modus-themes-load-theme 'ef-owl))

(use-package which-key :config (which-key-mode))

(use-package projectile
  :init (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package dirvish
  :init (dirvish-override-dired-mode)
  :bind
  (("<f6>" . dirvish-dwim))
  :config
  (setq dirvish-default-layout '(0 0.3 0.7)
        dirvish-attributes '(subtree-state collapse git-msg)))

(use-package ace-window :bind (("M-o" . ace-window)))
(use-package avy
  :bind (("C-:" . avy-goto-char-timer)
	 ("M-g -" . avy-kill-region)
	 ("M-g =" . avy-move-region)
	 ("M-g +" . avy-copy-region)))

(provide 'config-ui)
