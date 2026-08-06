;;; lang-rust.el --- Rust development setup -*- lexical-binding: t; -*-

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :hook (rust-mode . eglot-ensure))

(provide 'lang-rust)
