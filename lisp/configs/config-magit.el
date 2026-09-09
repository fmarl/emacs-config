;;; config-magit.el --- Magit related config -*- lexical-binding: t; -*-

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind (("C-c u" . magit-status-quick)))

(provide 'config-magit)
