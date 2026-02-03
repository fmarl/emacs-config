;;; lang-rust.el --- Rust development setup -*- lexical-binding: t; -*-

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :after (rust-mode)
  :init
  (setq rustic-lsp-server 'rust-analyzer
        rustic-format-on-save t
        rustic-lsp-client 'eglot)
  :hook
  (rustic-mode . eglot-ensure))

(with-eval-after-load 'rustic
  (define-key rustic-mode-map (kbd "C-c C-c c") #'rustic-cargo-check)
  (define-key rustic-mode-map (kbd "C-c C-c r") #'rustic-cargo-run))

(provide 'lang-rust)
