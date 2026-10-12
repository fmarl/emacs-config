;;; config-notify.el --- Desktop notifications -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package notifications
  :commands notifications-notify)

(defun my/buffer-in-focus-p (buffer)
  "Return non-nil if BUFFER is shown in a focused frame."
  (seq-some (lambda (window)
              (eq (frame-focus-state (window-frame window)) t))
            (get-buffer-window-list buffer nil t)))

(defun my/notify-compilation-finished (buffer status)
  "Announce that the compilation in BUFFER ended with STATUS.
Stay silent for grep buffers and while BUFFER is in view."
  (unless (or (with-current-buffer buffer (derived-mode-p 'grep-mode))
              (my/buffer-in-focus-p buffer))
    (notifications-notify :title (buffer-name buffer)
                          :body (string-trim status)
                          :urgency (if (string-prefix-p "finished" status)
                                       'normal
                                     'critical))))

(use-package compile
  :defer t
  :config
  (add-hook 'compilation-finish-functions #'my/notify-compilation-finished))

(provide 'config-notify)
