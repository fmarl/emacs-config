;;; lang-java.el --- Java development setup -*- lexical-binding: t; -*-

(use-package eglot-java
  :hook (java-mode . eglot-ensure))

(provide 'lang-java)
