;;; lang-scheme.el --- Guile and Guix development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package geiser-guile
  :if (executable-find "guix")
  :defer t
  :custom (geiser-guile-binary '("guix" "repl")))

(provide 'lang-scheme)

;;; lang-scheme.el ends here
