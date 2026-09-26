;;; config-editing.el --- Editing and formatting helpers -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package markdown-mode :mode "\\.md\\'")

(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode scheme-mode clojure-mode) . paredit-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package isearch
  :ensure nil
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

(use-package diff-hl
  :init (global-diff-hl-mode)
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(add-to-list 'auto-mode-alist '("\\.json\\'" . js-json-mode))

(dolist (entry '((json "\\.json\\'" . json-ts-mode)
                 (toml "\\.toml\\'" . toml-ts-mode)
                 (yaml "\\.ya?ml\\'" . yaml-ts-mode)))
  (when (treesit-language-available-p (car entry))
    (add-to-list 'auto-mode-alist (cdr entry))))

(provide 'config-editing)
