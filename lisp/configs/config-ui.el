;;; config-ui.el --- Theme, navigation and UI packages -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'my-cache)
(require 'filenotify)
(require 'xdg)

(use-package modus-themes
  :demand t
  :functions modus-themes-load-theme
  :bind
  (("M-<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select))
  :custom
  (modus-themes-mixed-fonts t))

(defvar my/theme-file (expand-file-name "theme" (xdg-config-home)))

(defun my/read-theme-file ()
  (ignore-error file-missing
    (with-temp-buffer
      (insert-file-contents my/theme-file)
      (intern (string-trim (buffer-string))))))

(defun my/load-file-theme ()
  "Load the theme named in `my/theme-file'."
  (when-let* ((theme (my/read-theme-file)))
    (cond ((not (memq theme (custom-available-themes)))
           (message "Unknown theme `%s' in %s" theme my/theme-file))
          ((not (custom-theme-enabled-p theme))
           (modus-themes-load-theme theme)))))

(defun my/theme-file-changed (event)
  (when (member my/theme-file (nthcdr 2 event))
    (my/load-file-theme)))

(my/load-file-theme)

;; Tools replace the file, so watch its directory
(let ((dir (file-name-directory my/theme-file)))
  (when (file-directory-p dir)
    (file-notify-add-watch dir '(change) #'my/theme-file-changed)))

(use-package which-key :config (which-key-mode))

(use-package dirvish
  :init (dirvish-override-dired-mode)
  :bind
  (("C-c d" . dirvish-dwim))
  :custom
  (dirvish-cache-dir (my/cache-file "dirvish/"))
  (dirvish-default-layout '(0 0.3 0.7))
  (dirvish-attributes '(subtree-state collapse)))

(use-package ace-window :bind (("C-c w" . ace-window)))

(use-package avy
  :bind (("M-j" . avy-goto-char-timer)
         ("M-g -" . avy-kill-region)
         ("M-g =" . avy-move-region)
         ("M-g +" . avy-copy-region)))

(provide 'config-ui)

;;; config-ui.el ends here
