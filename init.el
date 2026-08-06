;;; init.el --- Main configuration -*- lexical-binding: t; -*-

(defun my/backup-enable-p (name)
  (and (normal-backup-enable-predicate name)
       (not (string-match-p "/\\.\\(aws\\|ssh\\|gnupg\\)/\\|/secrets/\\|\\.env\\(\\.[^/]*\\)?\\'" name))))

(setq ;; packages come from Nix/Guix; package.el must never reach out
 package-archives nil
 use-package-always-ensure nil
 use-package-expand-minimally t
 read-process-output-max (* 1024 1024)
 inhibit-startup-screen t
 native-comp-async-report-warnings-errors nil
 ring-bell-function 'ignore
 load-prefer-newer t
 epg-pinentry-mode 'loopback
 network-security-level 'high

 ;; strip User-Agent/OS/version info, reject cookies
 url-privacy-level 'paranoid

 ;; never fetch remote images in shr buffers (elfeed, eww, HTML mail)
 shr-inhibit-images t
 backup-directory-alist '((".*" . "~/.cache/emacs/backups/"))
 backup-enable-predicate #'my/backup-enable-p
 auto-save-default nil

 ;; keep runtime state in ~/.cache instead of the config repo
 savehist-file "~/.cache/emacs/history"
 save-place-file "~/.cache/emacs/places"
 recentf-save-file "~/.cache/emacs/recentf"

 ;; both cleanups stat every entry, hanging on stale TRAMP paths
 recentf-auto-cleanup 'never
 save-place-forget-unreadable-files nil
 custom-file (expand-file-name "custom.el" user-emacs-directory))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-cursor-color "#ffffff")
(add-to-list 'default-frame-alist '(font . "Aporetic Sans Mono 14"))

(column-number-mode 1)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(global-prettify-symbols-mode 1)

(make-directory "~/.cache/emacs/" t)
(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)

(add-to-list 'load-path (expand-file-name "lisp/langs/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/configs/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "reka/" user-emacs-directory))

(mapc #'require '(config-my config-ui config-editing config-minibuffer
			    config-completion config-eglot config-org config-circe
			    config-magit config-mu4e config-elfeed config-meow config-reka))

(when (eq system-type 'darwin)
  (require 'config-lex))

;; envrc needs to be enabled late in init
(use-package envrc
  :config (envrc-global-mode 1))

(when (file-exists-p custom-file)
  (load custom-file))

(require 'config-reka)
(reka-enable)

(provide 'init)
