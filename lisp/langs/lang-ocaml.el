;;; lang-ocaml.el --- OCaml development setup -*- lexical-binding: t; -*-

(use-package tuareg
  :defer t
  :hook (tuareg-mode . eglot-ensure)
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package utop
  :config
  (add-hook 'tuareg-mode-hook #'utop-minor-mode))

(use-package dune)

(provide 'lang-ocaml)
