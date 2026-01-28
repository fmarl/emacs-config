;; -*- lexical-binding: t; -*-

(use-package eglot
  :ensure t
  :config
  (setq eglot-sync-connect nil
        eglot-autoshutdown t
        eglot-extend-to-xref t
	eglot-code-action-indicator "*>")
  	       
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (flymake-mode 1)
              (eldoc-mode 1))))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("M-." . xref-find-definitions)
              ("M-," . xref-pop-marker-stack)
	      ("M-?" . xref-find-references)
              ("C-c M-?" . consult-eglot-symbols)
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename)))
