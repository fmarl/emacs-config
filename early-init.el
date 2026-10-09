;;; early-init.el --- Pre-init setup -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Lowered again after startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024))))

(setq native-comp-async-report-warnings-errors 'silent)

(advice-add #'display-startup-echo-area-message :override #'ignore)

(setq frame-resize-pixelwise t)

(when (featurep 'native-compile)
  (startup-redirect-eln-cache "~/.cache/emacs/eln-cache/"))

(unless (eq system-type 'darwin)
  (push '(undecorated . t) default-frame-alist))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(font . "Aporetic Sans Mono 14") default-frame-alist)
(setq frame-inhibit-implied-resize t)
