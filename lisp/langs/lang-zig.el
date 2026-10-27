;;; lang-zig.el --- Zig development setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(use-package zig-mode
  :hook (zig-mode . eglot-ensure))

(provide 'lang-zig)

;;; lang-zig.el ends here
