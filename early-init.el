;;; early-init.el --- Pre-init setup -*- lexical-binding: t; -*-

;; Lowered again after startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 100 1024 1024))))

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

(advice-add #'display-startup-echo-area-message :override #'ignore)

(setq frame-resize-pixelwise t)

;; Disable UI elements before the first frame is drawn
(push '(undecorated . t) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)
