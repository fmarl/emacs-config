;;; lang-cc.el --- C/C++ development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package c-ts-mode
  :hook (c-ts-base-mode . eglot-ensure))

(provide 'lang-cc)

;;; lang-cc.el ends here
