;;; lang-typescript.el --- TypeScript/JavaScript development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package typescript-ts-mode
  :mode ("\\.[mc]ts\\'" . typescript-ts-mode)
  :hook ((typescript-ts-mode tsx-ts-mode) . eglot-ensure))

(use-package js
  :mode ("\\.[mc]js\\'" . js-ts-mode)
  :hook (js-ts-mode . eglot-ensure)
  :custom
  (js-indent-level 2))

(provide 'lang-typescript)

;;; lang-typescript.el ends here
