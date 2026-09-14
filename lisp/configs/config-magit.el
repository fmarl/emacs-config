;;; config-magit.el --- Magit related config -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind (("C-c u" . magit-status-quick)))

(provide 'config-magit)
