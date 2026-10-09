;;; config-eglot.el --- Eglot configuration -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package eglot
  :defer t
  :custom
  (eglot-sync-connect nil)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-code-action-indicator "*>")
  (eglot-events-buffer-config '(:size 0 :format full))
  :bind (:map eglot-mode-map
              ("C-c e a" . eglot-code-actions)
              ("C-c e r" . eglot-rename))
  :config
  (let ((inlay-hints '(:includeInlayParameterNameHints "all"
                       :includeInlayFunctionParameterTypeHints t
                       :includeInlayVariableTypeHints t
                       :includeInlayPropertyDeclarationTypeHints t
                       :includeInlayFunctionLikeReturnTypeHints t
                       :includeInlayEnumMemberValueHints t)))
    (setq-default
     eglot-workspace-configuration
     `(:gopls (:staticcheck t :gofumpt :json-false)
       :basedpyright (:analysis (:typeCheckingMode "standard"))
       :nil (:formatting (:command ["nixfmt"]))
       :typescript (:inlayHints ,inlay-hints)
       :javascript (:inlayHints ,inlay-hints)))))

(use-package consult-eglot
  :after eglot
  :bind (:map eglot-mode-map
              ("C-c e s" . consult-eglot-symbols)))

(provide 'config-eglot)
