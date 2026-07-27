;;; config-eglot.el --- Eglot configuration -*- lexical-binding: t; -*-

(use-package eglot
  :defer t
  :config
  (setq eglot-sync-connect nil
        eglot-autoshutdown t
        eglot-extend-to-xref t
	eglot-code-action-indicator "*>"))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("M-." . xref-find-definitions)
              ("M-," . xref-go-back)
	      ("M-?" . xref-find-references)
              ("C-c M-?" . consult-eglot-symbols)
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename)))

(provide 'config-eglot)
