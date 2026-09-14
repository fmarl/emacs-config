;;; lang-ocaml.el --- OCaml development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package tuareg
  :defer t
  :hook (tuareg-mode . eglot-ensure))

(use-package utop
  :config
  (add-hook 'tuareg-mode-hook #'utop-minor-mode))

(use-package dune)

(provide 'lang-ocaml)
