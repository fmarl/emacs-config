;;; config-mu4e.el --- My Mail config -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package mu4e
  :commands mu4e
  :custom
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300)
  (mu4e-index-lazy-check t)
  (mu4e-hide-index-messages t)
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-notification-support t)
  (mu4e-headers-date-format "%Y-%m-%d %H:%M")
  (mu4e-headers-fields '((:date . 20) (:flags . 6) (:from . 22) (:subject)))
  (mu4e-change-filenames-when-moving t)
  (mu4e-sent-folder "/Sent")
  (mu4e-drafts-folder "/Drafts")
  (mu4e-trash-folder "/Trash")
  (mu4e-refile-folder (lambda (msg)
                        (format-time-string "/Archive/%Y"
                                            (mu4e-message-field msg :date))))
  (mu4e-maildir-shortcuts '((:maildir "/INBOX" :key ?i)
                            (:maildir "/Sent" :key ?s)
                            (:maildir "/Archive" :key ?a)
                            (:maildir "/guix-devel" :key ?g)
                            (:maildir "/oss-security" :key ?o)))
  :config
  (setq mu4e-trash-without-flag t))

(use-package message
  :defer t
  :init (setq mail-user-agent 'mu4e-user-agent)
  :hook ((message-mode . turn-on-auto-fill)
         (message-setup . mml-secure-message-sign-pgpmime))
  :custom
  (user-full-name "Florian Marrero Liestmann")
  (user-mail-address "f.m.liestmann@fx-ttr.de")
  (message-dont-reply-to-names '("f\\.m\\.liestmann@fx-ttr\\.de"))
  (message-send-mail-function #'message-send-mail-with-sendmail)
  (sendmail-program "msmtp")
  (message-citation-line-function #'message-insert-formatted-citation-line)
  (message-citation-line-format "On %Y-%m-%d, %N wrote:\n")
  (message-kill-buffer-on-exit t)
  (mml-secure-openpgp-sign-with-sender t)
  (mm-discouraged-alternatives '("text/html")))

(provide 'config-mu4e)
