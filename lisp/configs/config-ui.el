;;; config-ui.el --- Theme, navigation and UI packages -*- lexical-binding: t; -*-

(display-time-mode 1)
(display-battery-mode 1)

(use-package ef-themes
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("M-<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select))
  :config
  (modus-themes-load-theme 'ef-owl))

(use-package which-key :config (which-key-mode))

(use-package dirvish
  :init (dirvish-override-dired-mode)
  :bind
  (("<f6>" . dirvish-dwim))
  :config
  (setq dirvish-default-layout '(0 0.3 0.7)
        dirvish-attributes '(subtree-state collapse git-msg)))

(setq eshell-prompt-function
      (lambda ()
	(let ((sh-level (- (string-to-number (getenv "SHLVL")) 1))
	      (aws-profile (getenv "AWS_PROFILE")))
	  (concat
	   (abbreviate-file-name default-directory)
	   " "
	   (if (> sh-level 0)
	       (concat "[" (number-to-string sh-level) "] ")
	     "")
	   (if (not (eq aws-profile nil))
	       (concat "[" aws-profile "] ")
	     "")
	   (if (= (user-uid) 0) "#" "λ")
	   " "))))

(use-package ace-window :bind (("M-o" . ace-window)))
(use-package avy
  :bind (("C-:" . avy-goto-char-timer)
	 ("M-g -" . avy-kill-region)
	 ("M-g =" . avy-move-region)
	 ("M-g +" . avy-copy-region)))

(provide 'config-ui)
