;;; lang-python.el --- Python development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package python
  :hook (python-base-mode . eglot-ensure))

(defvar apheleia-mode-alist)
(with-eval-after-load 'apheleia
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

(provide 'lang-python)

;;; lang-python.el ends here
