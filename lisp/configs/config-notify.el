;;; config-notify.el --- Desktop notifications -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package notifications
  :autoload notifications-notify)

(defun my/buffer-in-focus-p (buffer)
  (seq-some (lambda (window)
              (eq (frame-focus-state (window-frame window)) t))
            (get-buffer-window-list buffer nil t)))

(defun my/notify-compilation-finished (buffer status)
  (unless (or (with-current-buffer buffer (derived-mode-p 'grep-mode))
              (my/buffer-in-focus-p buffer))
    (with-demoted-errors "Notification failed: %S"
      (notifications-notify :title (buffer-name buffer)
                            :body (string-trim status)
                            :urgency (if (string-prefix-p "finished" status)
                                         'normal
                                       'critical)))))

(add-hook 'compilation-finish-functions #'my/notify-compilation-finished)

(provide 'config-notify)

;;; config-notify.el ends here
