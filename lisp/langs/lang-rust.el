;;; lang-rust.el --- Rust development setup -*- lexical-binding: t; -*-

(use-package rust-mode
  :ensure t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :ensure t
  :after (rust-mode)
  :init
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-format-on-save t)
  (setq rustic-lsp-client 'eglot)
  :hook
  (rustic-mode . eglot-ensure)
  :config
  (add-hook 'lsp-mode-hook #'yas-minor-mode)
  ;; configure inlay hints und andere Optionen
  (setq lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-display-chaining-hints t)
  )

(with-eval-after-load 'rustic
  (define-key rustic-mode-map (kbd "C-c C-c c") #'rustic-cargo-check)
  (define-key rustic-mode-map (kbd "C-c C-c r") #'rustic-cargo-run))

(provide 'lang-rust)
