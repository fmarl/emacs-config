;;; lang-ocaml.el --- OCaml development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package tuareg
  :mode (("\\.mlp?\\'" . tuareg-mode)
         ("\\.mli\\'" . tuareg-interface-mode)
         ("\\.ocamlinit\\'" . tuareg-mode))
  :hook (tuareg-mode . eglot-ensure))

(use-package utop
  :hook (tuareg-mode . utop-minor-mode))

(use-package dune :defer t)

(provide 'lang-ocaml)
