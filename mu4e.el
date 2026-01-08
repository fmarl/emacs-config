(use-package mu4e
  :ensure nil
  :defer 5
  :config
  (setq mu4e-maildir "~/Mail"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-compose-format-flowed t
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

  (setq mu4e-compose-format-flowed nil)
  (setq mm-discouraged-alternatives '("text/html"))
  (setq mu4e-compose-insert-old-message nil)

  (setq message-default-mail-headers
	"Content-Type: text/plain; charset=utf-8\n")

  (setq mu4e-compose-cite-style 'plain)
  (setq mu4e-compose-quote-style 'plain)
  (setq mu4e-compose-cite-function 'mu4e-compose-cite-original)

  (setq message-cite-reply-position 'below)
  (setq message-yank-prefix "> ")
  (setq message-yank-cited-prefix "> ")
  (setq message-yank-empty-prefix "> ")

  (setq message-citation-line-function
	'message-insert-formatted-citation-line)
  (setq message-citation-line-format
	"On %Y-%m-%d, %N wrote:\n")

  (setq message-fill-column 72)
  (add-hook 'message-mode-hook #'turn-on-auto-fill)
  (setq message-signature nil)

  (setq mu4e-compose-reply-to-address user-mail-address)
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-reply-keep-subject t)

  (setq mu4e-use-fancy-chars nil)
  (setq mu4e-attachment-dir nil)
  (setq mu4e-view-show-images nil)
  (setq mu4e-view-show-addresses t)

  (setq mu4e-sent-folder   "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder  "/Trash"
        mu4e-refile-folder "/Archive")

  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-action-view-in-browser) t)

  (setq mu4e-maildir-shortcuts
        '(("/INBOX"          . ?i)
          ("/linux-kernel"   . ?k)
          ("/linux-janitors" . ?j)
          ("/guix-devel"     . ?g)
          ("/Sent"           . ?s)
          ("/Archive"        . ?a)))

  (setq mu4e-headers-include-related t))
