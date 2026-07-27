;;; early-init.el --- Pre-init setup -*- lexical-binding: t; -*-

;; Do NOT set `package-enable-at-startup' to nil: nixpkgs ships packages
;; as site-lisp/elpa directories whose autoloads only get activated by
;; the automatic `package-activate-all' during startup.

;; Lowered again after startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 100 1024 1024))))

;; Disable UI elements before the first frame is drawn
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)
