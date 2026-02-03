;;; config-mu4e.el --- My Mail config -*- lexical-binding: t; -*-

(use-package mu4e
  :ensure nil
  :defer 5
  :config
  (setq mu4e-maildir "~/Mail"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300
        mu4e-headers-auto-update t
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-headers-fields '((:date . 20)
                              (:flags . 6)
                              (:from . 22)
                              (:subject)))

  (setq user-full-name      "Florian Marrero Liestmann"
        user-mail-address   "f.m.liestmann@fx-ttr.de")

  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp"
        mail-specify-envelope-from t
        mail-envelope-from 'header
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from"))

  (setq mu4e-compose-format-flowed nil
        mm-discouraged-alternatives '("text/html")
        mu4e-compose-insert-old-message nil
        message-default-mail-headers "Content-Type: text/plain; charset=utf-8\n"
        mu4e-compose-cite-style 'plain
        mu4e-compose-quote-style 'plain
        mu4e-compose-cite-function 'mu4e-compose-cite-original
        message-cite-reply-position 'below
        message-yank-prefix "> "
        message-yank-cited-prefix "> "
        message-yank-empty-prefix "> "
        message-citation-line-function 'message-insert-formatted-citation-line
        message-citation-line-format "On %Y-%m-%d, %N wrote:\n"
        message-fill-column 72
        message-signature nil
        mu4e-compose-reply-to-address user-mail-address
        mu4e-compose-dont-reply-to-self t
        mu4e-reply-keep-subject t)

  (add-hook 'message-mode-hook #'turn-on-auto-fill)

  (setq mu4e-use-fancy-chars nil
        mu4e-view-show-images nil
        mu4e-view-show-addresses t
        mu4e-change-filenames-when-moving t)

  (setq mu4e-sent-folder   "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder  "/Trash"
        mu4e-refile-folder "/Archive")

  (setq mu4e-maildir-shortcuts
        '(("/INBOX"          . ?i)
          ("/kernel-linux"   . ?k)
          ("/kernel-janitors" . ?j)
          ("/guix-devel"     . ?g)
          ("/Sent"           . ?s)
          ("/Archive"        . ?a))
        mu4e-headers-include-related t))

(provide 'config-mu4e)
