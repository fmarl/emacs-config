;;; lang-gleam.el --- Gleam development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package gleam-ts-mode
  :mode "\\.gleam\\'"
  :hook (gleam-ts-mode . eglot-ensure))

(defvar eglot-server-programs)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(gleam-ts-mode . ("gleam" "lsp"))))

(provide 'lang-gleam)

;;; lang-gleam.el ends here
