;;; lang-shell.el --- Shell scripting setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package sh-script
  :hook (bash-ts-mode . eglot-ensure))

(provide 'lang-shell)

;;; lang-shell.el ends here
