;;; lang-java.el --- Java development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package java-ts-mode
  :hook (java-ts-mode . eglot-ensure))

;; Re-enable per project in .dir-locals.el:
;;   ((java-ts-mode . ((apheleia-formatter . google-java-format))))
(defvar apheleia-mode-alist)
(with-eval-after-load 'apheleia
  (setf (alist-get 'java-mode apheleia-mode-alist nil t) nil
        (alist-get 'java-ts-mode apheleia-mode-alist nil t) nil))

(provide 'lang-java)

;;; lang-java.el ends here
