;;; lang-python.el --- Python development setup -*- lexical-binding: t; -*-

(use-package python
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :hook ((python-base-mode . eglot-ensure)
	 (python-base-mode . (lambda ()
                               (setq tab-width 4)
                               (setq python-indent-offset 4)))))

;; Note: Using Flymake from Eglot, not Flycheck to avoid conflicts

(provide 'lang-python)
