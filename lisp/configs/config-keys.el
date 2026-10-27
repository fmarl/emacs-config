;;; config-keys.el --- Global bindings for built-in commands -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "C-c f" #'find-file)
(keymap-global-set "C-c j" goto-map)
(keymap-global-set "C-c p" project-prefix-map)
(keymap-global-set "C-c s" search-map)

(provide 'config-keys)

;;; config-keys.el ends here
