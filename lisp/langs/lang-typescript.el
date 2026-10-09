;;; lang-typescript.el --- TypeScript/JavaScript development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(use-package typescript-ts-mode
  :mode (("\\.[mc]?ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode tsx-ts-mode) . eglot-ensure))

(use-package js
  :mode ("\\.[mc]?jsx?\\'" . js-ts-mode)
  :hook (js-ts-mode . eglot-ensure)
  :custom
  (js-indent-level 2))

(provide 'lang-typescript)
