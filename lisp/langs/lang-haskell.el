;;; lang-haskell.el --- Haskell development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package haskell-mode
  :hook (haskell-mode . eglot-ensure))

(provide 'lang-haskell)
