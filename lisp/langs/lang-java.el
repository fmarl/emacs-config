;;; lang-java.el --- Java development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun my/java-indent-setup ()
  "Indent with 4 spaces."
  (setq-local indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4))

(use-package cc-mode
  :ensure nil
  :hook (((java-mode java-ts-mode) . my/java-indent-setup)
         ((java-mode java-ts-mode) . eglot-ensure)))

(provide 'lang-java)
