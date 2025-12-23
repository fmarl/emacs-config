;;; lang-dart.el --- Dart/Flutter development setup -*- lexical-binding: t; -*-

(use-package dart-mode
  :ensure t
  :hook (dart-mode . eglot-ensure))

(use-package flutter
  :ensure t
  :hook (dart-mode . flutter-mode))

(provide 'lang-dart)
