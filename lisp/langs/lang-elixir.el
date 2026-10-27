;;; lang-elixir.el --- Elixir development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package elixir-ts-mode
  :hook ((elixir-ts-mode heex-ts-mode) . eglot-ensure))

;; nixpkgs installs elixir-ls as `elixir-ls', not as the
;; language_server.sh that eglot expects by default
(defvar eglot-server-programs)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((elixir-ts-mode heex-ts-mode) . ("elixir-ls"))))

(provide 'lang-elixir)

;;; lang-elixir.el ends here
