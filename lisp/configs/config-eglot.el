;;; config-eglot.el --- Eglot configuration -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package eglot
  :defer t
  :config
  (setq eglot-sync-connect nil
        eglot-autoshutdown t
        eglot-extend-to-xref t
        eglot-code-action-indicator "*>"
        eglot-events-buffer-config '(:size 0 :format full)))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("C-c e s" . consult-eglot-symbols)
              ("C-c e a" . eglot-code-actions)
              ("C-c e r" . eglot-rename)))

(provide 'config-eglot)
