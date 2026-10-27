;;; lang-lisp.el --- Common Lisp development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package sly
  :defer t
  :custom (inferior-lisp-program "sbcl"))

(declare-function sly-connect "sly" (host port &optional coding-system interactive-p))

(defun my/nucleotide-connect ()
  "Connect SLY to the nucleotide REPL."
  (interactive)
  (require 'sly)
  (condition-case nil
      (sly-connect "localhost" 4005)
    (file-error
     (user-error "nucleotide's REPL is closed, open it with Super+Space r"))))

(provide 'lang-lisp)

;;; lang-lisp.el ends here
