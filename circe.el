(use-package circe
  :ensure t
  :config
  (setq circe-default-realname "Florian M.L."
	circe-default-nick "fmarl"
	circe-default-user "fmarl")
  (setq circe-network-options
	'(("Libera Chat"
           :tls t
           :channels ("#guix")
           )
	  ("Furnet"
	   :tls t
	   :nick "PaX"
	   :user "PaX"
	   :realname "PaX"
	   :host "alicorn.furnet.org"
	   :port (6667 . 6697)
	   :channels ("#pool")))))
