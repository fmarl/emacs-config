;;; lang-cc.el --- C/C++ development setup -*- lexical-binding: t; -*-

(use-package cc-mode
  :ensure nil
  :hook ((c-mode c++-mode) . eglot-ensure))

(provide 'lang-cc)
