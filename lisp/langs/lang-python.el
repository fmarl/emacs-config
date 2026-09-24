;;; lang-python.el --- Python development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun my/python-indent-setup ()
  "Indent with 4 spaces."
  (setq-local indent-tabs-mode nil
              tab-width 4
              python-indent-offset 4))

(use-package python
  :ensure nil
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :hook ((python-base-mode . eglot-ensure)
         (python-base-mode . my/python-indent-setup)))

(provide 'lang-python)
