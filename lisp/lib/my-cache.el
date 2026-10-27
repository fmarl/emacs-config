;;; my-cache.el --- Location of state and cache files -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'xdg)

(defvar my/cache-directory (expand-file-name "emacs/" (xdg-cache-home)))

(defun my/cache-file (name)
  (expand-file-name name my/cache-directory))

(provide 'my-cache)

;;; my-cache.el ends here
