;;; lang-java.el --- Java development setup -*- lexical-binding: t; -*-

(use-package eglot-java
  :ensure t
  :hook (java-mode . eglot-ensure))

(provide 'lang-java)
