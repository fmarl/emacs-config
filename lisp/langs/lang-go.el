;;; lang-go.el --- Go development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package go-ts-mode
  :hook ((go-ts-mode go-mod-ts-mode) . eglot-ensure)
  :bind (:map go-ts-mode-map
              ("C-c g d" . dlv)
              ("C-c g f" . dlv-current-func)))

(use-package go-dlv
  :commands (dlv dlv-current-func))

(defvar apheleia-mode-alist)
(with-eval-after-load 'apheleia
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports))

(provide 'lang-go)

;;; lang-go.el ends here
