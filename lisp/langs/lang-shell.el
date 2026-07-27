;;; lang-shell.el --- Shell scripting setup -*- lexical-binding: t; -*-

(use-package sh-script
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  :hook (bash-ts-mode . eglot-ensure))

(provide 'lang-shell)
