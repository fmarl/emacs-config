;;; config-mu4e.el --- My Mail config -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package mu4e
  :commands mu4e
  :hook (message-mode . turn-on-auto-fill)
  :custom
  (user-full-name "Florian Marrero Liestmann")
  (user-mail-address "f.m.liestmann@fx-ttr.de")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 300)
  (mu4e-headers-auto-update t)
  (mu4e-headers-date-format "%Y-%m-%d %H:%M")
  (mu4e-headers-fields '((:date . 20) (:flags . 6) (:from . 22) (:subject)))
  (mu4e-headers-include-related t)
  (mu4e-use-fancy-chars nil)
  (mu4e-view-show-addresses t)
  (mu4e-change-filenames-when-moving t)
  (mu4e-compose-format-flowed nil)
  (mu4e-compose-reply-to-address "f.m.liestmann@fx-ttr.de")
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-sent-folder "/ionos/Gesendete Objekte")
  (mu4e-drafts-folder "/ionos/Entwürfe")
  (mu4e-trash-folder "/ionos/Papierkorb")
  (mu4e-refile-folder "/ionos/Archive")
  (mu4e-maildir-shortcuts '((:maildir "/ionos/Inbox" :key ?i)
                            (:maildir "/ionos/Gesendete Objekte" :key ?s)
                            (:maildir "/ionos/Archive" :key ?a)))
  (message-send-mail-function #'message-send-mail-with-sendmail)
  (sendmail-program "msmtp")
  (mail-specify-envelope-from t)
  (mail-envelope-from 'header)
  (message-sendmail-f-is-evil t)
  (message-sendmail-extra-arguments '("--read-envelope-from"))
  (message-default-mail-headers "Content-Type: text/plain; charset=utf-8\n")
  (message-cite-reply-position 'below)
  (message-yank-prefix "> ")
  (message-yank-cited-prefix "> ")
  (message-yank-empty-prefix "> ")
  (message-citation-line-function #'message-insert-formatted-citation-line)
  (message-citation-line-format "On %Y-%m-%d, %N wrote:\n")
  (message-fill-column 72)
  (message-signature nil)
  (mm-discouraged-alternatives '("text/html")))

(provide 'config-mu4e)
