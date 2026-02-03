;;; init.el --- Main configuration -*- lexical-binding: t; -*-

(require 'package)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t
      use-package-expand-minimally t
      use-package-compute-statistics t
      custom-safe-themes t)

;; Increase GC threshold during startup for better performance
(setq gc-cons-threshold most-positive-fixnum)

(setq read-process-output-max (* 1024 1024)
      inhibit-startup-screen t
      native-comp-async-report-warnings-errors nil)

;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024))))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; GPG/EPG Configuration
(require 'epg)
(setq epg-pinentry-mode 'loopback)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(global-prettify-symbols-mode 1)
(set-cursor-color "#ffffff")
(set-frame-font "Aporetic Sans Mono 12" nil t)
(setq ring-bell-function 'ignore)
(setq enable-local-variables :safe)

;; Kill the last word instead of using backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-sl" 'scroll-lock-mode)
(global-set-key "\C-c\C-sa" 'scroll-all-mode)

;;; -------------------------

;; Backup und Auto-Save
(setq backup-directory-alist `((".*" . ,(expand-file-name "backups/" user-emacs-directory)))
      auto-save-default nil)

;; Theme
(use-package ef-themes
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("M-<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select))
  :config
  (modus-themes-load-theme 'ef-owl))

;; Which-key (Shortcut-Hilfe)
(use-package which-key :config (which-key-mode))

;; Projectile (Projektmanagement)
(use-package projectile
  :init (projectile-mode +1)
  :bind-keymap ("C-c p" . projectile-command-map))

;; File Explorer
(use-package dirvish
  :init (dirvish-override-dired-mode)
  :bind
  (("<f6>" . dirvish-dwim))
  :config
  (setq dirvish-default-layout '(0 0.3 0.7)
        dirvish-attributes '(subtree-state collapse git-msg)))

;; Ace / Jump Navigation
(use-package ace-window :bind (("M-o" . ace-window)))
(use-package avy
  :bind (("C-:" . avy-goto-char-timer)
	 ("M-g -" . avy-kill-region)
	 ("M-g =" . avy-move-region)
	 ("M-g +" . avy-copy-region)))

;; Markdown
(use-package markdown-mode :mode "\\.md\\'")

;; Direnv
(use-package direnv
  :config (direnv-mode))

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)	;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)		;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)	 ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-f d" . consult-find) ;; Alternative: consult-fd
         ("M-f c" . consult-locate)
         ("M-f g" . consult-grep)
         ("M-f G" . consult-git-grep)
         ("M-f r" . consult-ripgrep)
         ("M-f l" . consult-line)
         ("M-f L" . consult-line-multi)
         ("M-f k" . consult-keep-lines)
         ("M-f u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)	;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history) ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (define-prefix-command 'my/search-map)
  (global-set-key (kbd "M-f") 'my/search-map)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (setq consult-narrow-key "<"
        consult-ripgrep-command "rg --null --line-buffered --color=never --max-columns=1000 --path-separator --smart-case --no-heading --with-filename --line-number --search-zip --hidden --glob '!.git/*' --glob '!.direnv/*'"))

(use-package vertico
  :init (vertico-mode))

;; Use js-json-mode for JSON files (built-in, no auto-formatting)
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-json-mode))

;; Add langs and configs to modules to load path
(add-to-list 'load-path (expand-file-name "lisp/langs/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/configs/" user-emacs-directory))

(dolist (config '("my" "completion" "eglot" "org" "circe" "magit" "mu4e" "elfeed" "meow"))
  (require (intern (concat "config-" config))))

(when (string-match "darwin" (emacs-version))
  (require (intern "config-lex")))

;; Keep custom settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
