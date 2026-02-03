;;; lang-zig.el --- Zig development setup -*- lexical-binding: t; -*-

(use-package zig-mode
  :hook (zig-mode . eglot-ensure))

(provide 'lang-zig)
