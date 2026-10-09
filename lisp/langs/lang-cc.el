;;; lang-cc.el --- C/C++ development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package cc-mode
  :hook ((c-mode c++-mode) . eglot-ensure))

(provide 'lang-cc)
