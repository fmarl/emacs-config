;;; lang-go.el --- Go development setup -*- lexical-binding: t; -*-

(use-package go-ts-mode
  :ensure nil
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
  :hook ((go-ts-mode go-mod-ts-mode) . eglot-ensure))

(provide 'lang-go)
