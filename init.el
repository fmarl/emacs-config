;;; init.el --- Main configuration -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'use-package)
(require 'my-cache)

(setq package-archives nil)
(setopt use-package-expand-minimally t)

(set-language-environment "UTF-8")

(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(defun my/secret-file-p (name)
  (string-match-p
   (rx (or (seq "/." (or "aws" "gnupg" "kube" "password-store" "ssh") "/")
           "/secrets/"
           (seq "/." (or "authinfo" "netrc") (? ".gpg") eos)
           (seq ".env" (? "." (* (not "/"))) eos)))
   name))

(defun my/backup-enable-p (name)
  (and (normal-backup-enable-predicate name)
       (not (my/secret-file-p name))))

(defun my/disable-auto-save-for-secrets ()
  (when (my/secret-file-p buffer-file-name)
    (auto-save-mode -1)))

(add-hook 'find-file-hook #'my/disable-auto-save-for-secrets)

(setopt custom-file (expand-file-name "custom.el" user-emacs-directory)
        backup-directory-alist `(("." . ,(my/cache-file "backups/")))
        backup-enable-predicate #'my/backup-enable-p
        backup-by-copying t
        auto-save-file-name-transforms `((".*" ,(my/cache-file "auto-save/") t))
        auto-save-list-file-prefix (my/cache-file "auto-save/.saves-")
        read-process-output-max (* 1024 1024)
        inhibit-startup-screen t
        ring-bell-function #'ignore
        load-prefer-newer t
        indent-tabs-mode nil
        ffap-machine-p-known 'reject
        window-combination-resize t
        split-window-preferred-direction 'longest
        sentence-end-double-space nil
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
        kill-do-not-save-duplicates t
        save-interprogram-paste-before-kill t
        treesit-font-lock-level 4
        treesit-auto-install-grammar 'never
        treesit-enabled-modes '( bash-ts-mode
                                 c-ts-mode c++-ts-mode c-or-c++-ts-mode
                                 elixir-ts-mode heex-ts-mode
                                 go-ts-mode go-mod-ts-mode
                                 java-ts-mode
                                 js-ts-mode typescript-ts-mode tsx-ts-mode
                                 json-ts-mode toml-ts-mode yaml-ts-mode
                                 python-ts-mode
                                 rust-ts-mode))

(make-directory (my/cache-file "auto-save/") t)

(column-number-mode)
(blink-cursor-mode -1)
(pixel-scroll-precision-mode)
(repeat-mode)
(global-hl-line-mode)
(electric-pair-mode)
(global-prettify-symbols-mode)
(global-so-long-mode)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

(use-package autorevert
  :custom
  (auto-revert-avoid-polling t)
  (auto-revert-interval 5)
  :config (global-auto-revert-mode))

(use-package savehist
  :custom (savehist-file (my/cache-file "history"))
  :config (savehist-mode))

(use-package recentf
  :custom
  (recentf-save-file (my/cache-file "recentf"))
  (recentf-auto-cleanup 'never)
  :config (recentf-mode))

(use-package saveplace
  :custom
  (save-place-file (my/cache-file "places"))
  (save-place-forget-unreadable-files nil)
  :config (save-place-mode))

(use-package bookmark
  :defer t
  :custom
  (bookmark-default-file (my/cache-file "bookmarks"))
  (bookmark-save-flag 1))

(use-package project
  :defer t
  :custom (project-list-file (my/cache-file "projects")))

(use-package tramp
  :defer t
  :custom (tramp-persistency-file-name (my/cache-file "tramp")))

(use-package eshell
  :defer t
  :custom (eshell-directory-name (my/cache-file "eshell/")))

(use-package nsm
  :defer t
  :custom
  (nsm-settings-file (my/cache-file "network-security.data"))
  (network-security-level 'high))

(use-package epg-config
  :defer t
  :custom (epg-pinentry-mode 'loopback))

(use-package url-vars
  :defer t
  :custom (url-privacy-level 'paranoid))

(use-package shr
  :defer t
  :custom (shr-inhibit-images t))

(require 'config-keys)
(require 'config-ui)
(require 'config-editing)
(require 'config-minibuffer)
(require 'config-completion)
(require 'config-eglot)
(require 'config-org)
(require 'config-circe)
(require 'config-magit)
(require 'config-elfeed)

(require 'lang-cc)
(require 'lang-clojure)
(require 'lang-elixir)
(require 'lang-gleam)
(require 'lang-go)
(require 'lang-haskell)
(require 'lang-java)
(require 'lang-nasm)
(require 'lang-nix)
(require 'lang-ocaml)
(require 'lang-python)
(require 'lang-rust)
(require 'lang-shell)
(require 'lang-typescript)
(require 'lang-zig)

(if (eq system-type 'darwin)
    (require 'config-lex)
  (require 'config-mu4e)
  (require 'config-notify))

(use-package envrc
  :config (envrc-global-mode))

(load custom-file 'noerror)

(provide 'init)

;;; init.el ends here
