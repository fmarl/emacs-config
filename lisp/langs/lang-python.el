;;; lang-python.el --- Python development setup -*- lexical-binding: t; -*-

(use-package python
  :ensure nil
  :hook ((python-mode . eglot-ensure)
	 (python-mode . (lambda ()
                          (setq tab-width 4)
                          (setq python-indent-offset 4))))
  :config
  ;; Autoformat using black on save
  (use-package blacken
    :hook (python-mode . blacken-mode)
    :custom
    (blacken-allow-py36 t)
    (blacken-line-length 88)))

;; Note: Using Flymake from Eglot, not Flycheck to avoid conflicts

(provide 'lang-python)
