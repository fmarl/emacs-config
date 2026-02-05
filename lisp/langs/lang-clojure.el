;;; lang-clojure.el --- Clojure development setup -*- lexical-binding: t; -*-

(use-package cider
  :hook ((cider-mode) . paredit-mode))

(provide 'lang-clojure)
