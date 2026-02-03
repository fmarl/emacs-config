;;; lang-dart.el --- Dart/Flutter development setup -*- lexical-binding: t; -*-

(use-package dart-mode
  :hook (dart-mode . eglot-ensure))

(use-package flutter
  :hook (dart-mode . flutter-mode))

(provide 'lang-dart)
