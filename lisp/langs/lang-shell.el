;;; lang-shell.el --- Shell scripting setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package sh-script
  :init
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  :hook (bash-ts-mode . eglot-ensure))

(provide 'lang-shell)
