;;; lang-java.el --- Java development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package cc-mode
  :ensure nil
  :hook ((java-mode java-ts-mode) . eglot-ensure))

(provide 'lang-java)
