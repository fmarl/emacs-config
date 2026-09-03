;;; lang-rust.el --- Rust development setup -*- lexical-binding: t; -*-

(use-package rust-ts-mode
  :ensure nil
  :hook (rust-ts-mode . eglot-ensure))

(provide 'lang-rust)
