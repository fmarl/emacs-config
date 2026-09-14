;;; lang-elixir.el --- Elixir development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package elixir-ts-mode
  :mode (("\\.ex\\'" . elixir-ts-mode)
	 ("\\.exs\\'" . elixir-ts-mode)
	 ("mix\\.lock\\'" . elixir-ts-mode))
  :hook ((elixir-ts-mode heex-ts-mode) . eglot-ensure))

(use-package heex-ts-mode
  :mode "\\.[hl]?eex\\'")

;; nixpkgs installs elixir-ls as `elixir-ls', not as the
;; language_server.sh that eglot expects by default
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       '((elixir-ts-mode heex-ts-mode) . ("elixir-ls"))))

(provide 'lang-elixir)
