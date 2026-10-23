;;; config-ui.el --- Theme, navigation and UI packages -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'filenotify)
(require 'xdg)

(use-package modus-themes
  :demand t
  :bind
  (("M-<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)))

(defvar my/theme-file (expand-file-name "theme" (xdg-config-home)))

(defun my/load-file-theme (&rest _)
  "Load the theme named in `my/theme-file'."
  (when-let* ((name (ignore-errors
                      (with-temp-buffer
                        (insert-file-contents my/theme-file)
                        (string-trim (buffer-string)))))
              (theme (intern-soft name))
              ((memq theme (custom-available-themes))))
    (unless (custom-theme-enabled-p theme)
      (modus-themes-load-theme theme))))

(my/load-file-theme)

(when (file-exists-p my/theme-file)
  (file-notify-add-watch my/theme-file '(change) #'my/load-file-theme))

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
