;;; lang-go.el --- Go development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun my/go-indent-setup ()
  "Indent with tabs, as gofmt does."
  (setq-local indent-tabs-mode t
              tab-width 8))

;; go-dlv loads go-mode, which claims .go and go.mod for itself
(use-package go-dlv
  :commands (dlv dlv-current-func))

(use-package go-ts-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
  (add-to-list 'major-mode-remap-alist '(go-dot-mod-mode . go-mod-ts-mode))
  :hook (((go-ts-mode go-mod-ts-mode) . my/go-indent-setup)
         ((go-ts-mode go-mod-ts-mode) . eglot-ensure))
  :bind (:map go-ts-mode-map
              ("C-c g d" . dlv)
              ("C-c g f" . dlv-current-func))
  :config
  (with-eval-after-load 'apheleia
    (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports)))

(provide 'lang-go)
