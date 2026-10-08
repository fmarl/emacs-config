;;; config-ui.el --- Theme, navigation and UI packages -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package modus-themes
  :demand t
  :bind
  (("M-<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select))
  :config
  (modus-themes-load-theme 'modus-operandi-tinted))

(use-package which-key :config (which-key-mode))

(use-package dirvish
  :init (dirvish-override-dired-mode)
  :bind
  (("C-c d" . dirvish-dwim))
  :custom
  (dirvish-default-layout '(0 0.3 0.7))
  (dirvish-attributes '(subtree-state collapse)))

(use-package ace-window :bind (("C-c w" . ace-window)))
(use-package avy
  :bind (("M-g -" . avy-kill-region)
         ("M-g =" . avy-move-region)
         ("M-g +" . avy-copy-region)))

(provide 'config-ui)
