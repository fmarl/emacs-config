;;; config-editing.el --- Editing and formatting helpers -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun my/kill-region-or-line (arg)
  "Kill the active region, or ARG whole lines if none is active."
  (interactive "p")
  (if (use-region-p)
      (kill-region nil nil t)
    (kill-whole-line arg)))

(defun my/copy-region-or-line (arg)
  "Copy the active region, or ARG lines if none is active."
  (interactive "p")
  (if (use-region-p)
      (kill-ring-save nil nil t)
    (kill-ring-save (line-beginning-position)
                    (line-beginning-position (1+ arg)))))

(keymap-global-set "C-w" #'my/kill-region-or-line)
(keymap-global-set "M-w" #'my/copy-region-or-line)

(use-package markdown-mode :defer t)

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
  (isearch-lazy-count-prefix-format "(%s/%s) ")
  (isearch-lazy-count t)
  (isearch-allow-motion t)
  (isearch-allow-scroll t)
  (isearch-repeat-on-direction-change t)
  (isearch-wrap-pause 'no-ding))

(use-package apheleia
  :init (apheleia-global-mode 1))

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

(provide 'config-editing)

;;; config-editing.el ends here
