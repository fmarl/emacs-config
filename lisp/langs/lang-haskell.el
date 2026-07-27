;;; lang-haskell.el --- Haskell development setup -*- lexical-binding: t; -*-

(use-package haskell-mode
  :hook (haskell-mode . eglot-ensure))

(provide 'lang-haskell)
