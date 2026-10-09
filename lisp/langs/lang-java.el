;;; lang-java.el --- Java development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun my/java-indent-setup ()
  "Indent with 4 spaces."
  (setq-local indent-tabs-mode nil
              tab-width 4))

(use-package java-ts-mode
  :init
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
  :hook ((java-ts-mode . my/java-indent-setup)
         (java-ts-mode . eglot-ensure)))

;; Re-enable per project in .dir-locals.el:
;;   ((java-ts-mode . ((apheleia-formatter . google-java-format))))
(with-eval-after-load 'apheleia
  (setf (alist-get 'java-mode apheleia-mode-alist nil t) nil
        (alist-get 'java-ts-mode apheleia-mode-alist nil t) nil))

(provide 'lang-java)
