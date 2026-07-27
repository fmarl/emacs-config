;;; lang-go.el --- Go development setup -*- lexical-binding: t; -*-

(use-package go-ts-mode
  :ensure nil
  :hook ((go-ts-mode go-mod-ts-mode) . eglot-ensure))

(provide 'lang-go)
