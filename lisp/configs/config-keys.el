;;; config-keys.el --- Personal key map -*- lexical-binding: t; -*-

(keymap-global-set "C-c f" #'find-file)
(keymap-global-set "C-c j" goto-map)
(keymap-global-set "C-c p" project-prefix-map)
(keymap-global-set "C-c s" search-map)

(provide 'config-keys)
