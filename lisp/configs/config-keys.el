;;; config-keys.el --- Personal key map -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(keymap-global-set "C-x C-b" #'ibuffer)
(keymap-global-set "C-c f" #'find-file)
(keymap-global-set "C-c j" goto-map)
(keymap-global-set "C-c p" project-prefix-map)
(keymap-global-set "C-c s" search-map)

(provide 'config-keys)
