;;; lang-go.el --- Go development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun my/go-indent-setup ()
  "Indent with tabs, as gofmt does."
  (setq-local indent-tabs-mode t
              tab-width 8))

(use-package go-ts-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
  :hook (((go-ts-mode go-mod-ts-mode) . my/go-indent-setup)
         ((go-ts-mode go-mod-ts-mode) . eglot-ensure)))

(provide 'lang-go)
