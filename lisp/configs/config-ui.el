;;; config-ui.el --- Theme, navigation and UI packages -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package modus-themes
  :demand t
  :bind
  (("M-<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select))
  :custom
  (modus-themes-to-toggle '(modus-operandi-tinted ef-owl)))

(use-package ef-themes :demand t)

(defun my/load-appearance-theme (appearance)
  "Load the dark or light theme of `modus-themes-to-toggle' for APPEARANCE.
Fall back to the light one when the dark theme is not installed."
  (pcase-let ((`(,light ,dark) modus-themes-to-toggle))
    (modus-themes-load-theme
     (if (and (eq appearance 'dark) (memq dark (custom-available-themes)))
         dark
       light))))

(require 'xdg)

(let ((file (expand-file-name "theme" (xdg-config-home))))
  (if (file-readable-p file)
      (modus-themes-load-theme (with-temp-buffer
                                 (insert-file-contents file)
                                 (read (current-buffer))))
    (my/load-appearance-theme (bound-and-true-p ns-system-appearance))
    (when (boundp 'ns-system-appearance-change-functions)
      (add-hook 'ns-system-appearance-change-functions
                #'my/load-appearance-theme))))

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
