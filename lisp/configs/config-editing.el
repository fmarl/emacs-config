;;; config-editing.el --- Editing and formatting helpers -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package markdown-mode :mode "\\.md\\'")

(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode scheme-mode clojure-mode) . paredit-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package editorconfig
  :config (editorconfig-mode))

(use-package isearch
  :bind (:map isearch-mode-map
              ("C-." . isearch-forward-thing-at-point))
  :custom
  (lazy-count-prefix-format "(%s/%s) ")
  (isearch-lazy-count t)
  (isearch-allow-motion t)
  (isearch-allow-scroll t)
  (isearch-repeat-on-direction-change t)
  (isearch-wrap-pause 'no-ding))

(use-package apheleia
  :init (apheleia-global-mode 1)
  :config
  ;; apheleia defaults terraform-mode to opentofu
  (setf (alist-get 'terraform-mode apheleia-mode-alist) 'terraform))

(use-package ediff
  :defer t
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package compile
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package diff-hl
  :init (global-diff-hl-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(provide 'config-editing)
