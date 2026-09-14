;;; config-circe.el --- My IRC client configuration -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package circe
  :defer t
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
