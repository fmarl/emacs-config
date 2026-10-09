;;; init.el --- Main configuration -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(add-to-list 'load-path (expand-file-name "lisp/langs/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/configs/" user-emacs-directory))

(setq package-archives nil
      use-package-always-ensure nil
      use-package-expand-minimally t)

(defun my/secret-file-p (name)
  "Return non-nil if NAME looks like it holds secrets."
  (string-match-p "/\\.\\(aws\\|ssh\\|gnupg\\)/\\|/secrets/\\|\\.env\\(\\.[^/]*\\)?\\'" name))

(defun my/backup-enable-p (name)
  "Return non-nil if NAME should be backed up.
Like `normal-backup-enable-predicate', but also reject secrets."
  (and (normal-backup-enable-predicate name)
       (not (my/secret-file-p name))))

(defun my/disable-auto-save-for-secrets ()
  "Turn off auto-save if the visited file looks like it holds secrets."
  (when (my/secret-file-p buffer-file-name)
    (auto-save-mode -1)))

(setq
 read-process-output-max (* 1024 1024)
 inhibit-startup-screen t
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
 auto-save-file-name-transforms '((".*" "~/.cache/emacs/auto-save/" t))
 auto-save-list-file-prefix "~/.cache/emacs/auto-save/.saves-"
 savehist-file "~/.cache/emacs/history"
 save-place-file "~/.cache/emacs/places"
 recentf-save-file "~/.cache/emacs/recentf"
 bookmark-default-file "~/.cache/emacs/bookmarks"
 project-list-file "~/.cache/emacs/projects"
 tramp-persistency-file-name "~/.cache/emacs/tramp"
 nsm-settings-file "~/.cache/emacs/network-security.data"
 org-id-locations-file "~/.cache/emacs/org-id-locations"
 eshell-directory-name "~/.cache/emacs/eshell/"
 transient-history-file "~/.cache/emacs/transient/history.el"
 transient-levels-file "~/.cache/emacs/transient/levels.el"
 transient-values-file "~/.cache/emacs/transient/values.el"
 dirvish-cache-dir "~/.cache/emacs/dirvish/"
 elfeed-db-directory "~/.cache/emacs/elfeed/"
 custom-file (expand-file-name "custom.el" user-emacs-directory)

 ;; both cleanups stat every entry, hanging on stale TRAMP paths
 recentf-auto-cleanup 'never
 save-place-forget-unreadable-files nil)

(make-directory "~/.cache/emacs/auto-save/" t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq-default bidi-paragraph-direction 'left-to-right
              indent-tabs-mode nil)
(setq bidi-inhibit-bpa t)

(setopt auto-revert-avoid-polling t
        auto-revert-interval 5
        ffap-machine-p-known 'reject
        window-combination-resize t
        split-window-preferred-direction 'longest
        sentence-end-double-space nil
        column-number-mode t
        mode-line-collapse-minor-modes nil
        display-line-numbers-width 3
        indicate-buffer-boundaries 'left
        global-hl-line-sticky-flag 'window
        show-paren-delay 0
        show-paren-style 'expression
        show-paren-context-when-offscreen 'overlay
        tab-bar-show 1
        use-short-answers t
        use-dialog-box nil
        read-extended-command-predicate #'command-completion-default-include-p
        vc-follow-symlinks t
        backup-by-copying t
        bookmark-save-flag 1
        kill-do-not-save-duplicates t
        save-interprogram-paste-before-kill t
        treesit-font-lock-level 4)

(global-auto-revert-mode)
(blink-cursor-mode -1)
(pixel-scroll-precision-mode)
(repeat-mode)
(global-hl-line-mode)
(electric-pair-mode)
(global-prettify-symbols-mode)
(global-so-long-mode)
(savehist-mode)
(recentf-mode)
(save-place-mode)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'find-file-hook #'my/disable-auto-save-for-secrets)

(mapc #'require '(config-keys config-ui config-editing config-minibuffer
                              config-completion config-eglot config-org config-circe
                              config-magit config-elfeed config-meow
                              lang-cc lang-clojure lang-elixir lang-gleam lang-go
                              lang-haskell lang-java lang-nasm lang-nix lang-ocaml
                              lang-python lang-rust lang-shell lang-zig))

(if (eq system-type 'darwin)
    (require 'config-lex)
  (require 'config-mu4e))

;; envrc needs to be enabled late in init
(use-package envrc
  :config (envrc-global-mode))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
