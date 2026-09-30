;;; init.el --- Main configuration -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun my/backup-enable-p (name)
  (and (normal-backup-enable-predicate name)
       (not (string-match-p "/\\.\\(aws\\|ssh\\|gnupg\\)/\\|/secrets/\\|\\.env\\(\\.[^/]*\\)?\\'" name))))

;; Automatically reread from disk if the underlying file changes by
;; using the OS file change notification interface rather than
;; repeatedly polling to see if there are changes.
;;
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
;; Set this to `nil' if Emacs is having trouble picking up changes.
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(global-auto-revert-mode)

;; Don't ping url-looking things when running find-file
(setopt ffap-machine-p-known 'reject)

;; Rebalance windows automatically when splitting
(setopt window-combination-resize t)

;; Prefer horizontal split on landscape monitors: `longest' is
;; default; can be `vertical' or `horizontal'.
;; See also the variable `split-width-threshold'.
(setopt split-window-preferred-direction 'longest)

;; Fix archaic defaults; justification: https://practicaltypography.com/one-space-between-sentences.html
(setopt sentence-end-double-space nil)

;; Basic speedups
;;
;; Emacs works really hard to be incredibly compatible out-of-the-box
;; with a wide variety of languages. That comes at the cost of a
;; little performance. These tell Emacs to assume left-to-right text
;; in all buffers.
;; Remove/comment if you read right-to-left languages (Arabic, Hebrew, etc.)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq-default indent-tabs-mode nil)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

(setq
 package-archives nil
 use-package-always-ensure nil
 use-package-expand-minimally t
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

 ;; keep runtime state in ~/.cache instead of the config repo
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

 ;; both cleanups stat every entry, hanging on stale TRAMP paths
 recentf-auto-cleanup 'never
 save-place-forget-unreadable-files nil
 custom-file (expand-file-name "custom.el" user-emacs-directory))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(add-to-list 'default-frame-alist '(font . "Aporetic Sans Mono 14"))

(setopt line-number-mode t)
(setopt column-number-mode t)
(setopt mode-line-collapse-minor-modes nil)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Makes it easier to repeat commands; `C-x o C-x o' becomes `C-x o o'
;; See https://karthinks.com/software/it-bears-repeating/
(repeat-mode)

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

(setopt global-hl-line-sticky-flag 'window) ; Every window gets own hl-line instance
(global-hl-line-mode)

;; Show matching delimiters
(setopt show-paren-delay 0)
(setopt show-paren-mode t)
(setopt show-paren-style 'expression)   ; default is 'parenthesis and just does delimiters
(setopt show-paren-context-when-offscreen 'overlay)
(electric-pair-mode 1)

(global-prettify-symbols-mode 1)

(make-directory "~/.cache/emacs/auto-save/" t)

(add-hook 'find-file-hook
          (lambda ()
            (unless (my/backup-enable-p buffer-file-name)
              (auto-save-mode -1))))
(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)

;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)

(setopt use-short-answers t
        use-dialog-box nil
        read-extended-command-predicate #'command-completion-default-include-p
        vc-follow-symlinks t
        backup-by-copying t
        bookmark-save-flag 1
        kill-do-not-save-duplicates t
        save-interprogram-paste-before-kill t
        treesit-font-lock-level 4)
(global-so-long-mode)

(add-to-list 'load-path (expand-file-name "lisp/langs/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/configs/" user-emacs-directory))

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
  :config (envrc-global-mode 1))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
