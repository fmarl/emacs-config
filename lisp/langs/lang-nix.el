;;; lang-nix.el --- Nix development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :hook (nix-ts-mode . eglot-ensure))

;; eglot only knows nix-mode, and nix-ts-mode does not derive from it
(defvar eglot-server-programs)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil"))))

(provide 'lang-nix)

;;; lang-nix.el ends here
