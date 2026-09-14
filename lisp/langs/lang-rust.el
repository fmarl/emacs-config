;;; lang-rust.el --- Rust development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package rust-ts-mode
  :ensure nil
  :hook (rust-ts-mode . eglot-ensure))

(provide 'lang-rust)
