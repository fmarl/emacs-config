;;; config-completion.el --- Completion-frontend config -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(setopt enable-recursive-minibuffers t)
(setopt completion-cycle-threshold 1)
(setopt tab-always-indent 'complete)
(setopt text-mode-ispell-word-completion nil)

(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-preview-current nil))

(use-package cape
  :after corfu
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package yasnippet
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :functions yas-reload-all
  :config (yas-reload-all))

(use-package yasnippet-snippets :after yasnippet)

(provide 'config-completion)
