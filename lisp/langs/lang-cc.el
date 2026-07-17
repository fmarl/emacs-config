;;; lang-cc.el --- C/C++ development setup -*- lexical-binding: t; -*-

(use-package cc-mode
  :ensure nil
  :hook (((c-mode c++-mode) . eglot-ensure)
	 ((c-mode c++-mode) . (lambda ()
				(setq c-file-style "linux")
				(setq indent-tabs-mode t)
				(setq tab-width 8)
				(setq fill-column 80)
				(setq c-basic-offset 4)
				(c-set-offset 'substatement-open 0)))))

(provide 'lang-cc)
