;;; config-circe.el --- My IRC client configuration -*- lexical-binding: t; -*-

(use-package circe
  :config
  (setq circe-default-realname "fmarl"
	circe-default-nick "fmarl"
	circe-default-user "fmarl")
  (setq circe-network-options
	'(("OTW"
           :tls t
	   :host "ircs.overthewire.org"
	   :port 6697
           )
	  ("Furnet"
	   :tls t
	   :nick "PaX"
	   :user "PaX"
	   :realname "PaX"
	   :host "alicorn.furnet.org"
	   :port (6667 . 6697)
	   :channels ("#pool")))))

(provide 'config-circe)
