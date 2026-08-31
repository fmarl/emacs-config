;;; config-minibuffer.el --- Minibuffer completion stack -*- lexical-binding: t; -*-

(use-package consult
  :bind (("C-c b" . consult-buffer)
         ("C-c r" . consult-ripgrep)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         ([remap imenu] . consult-imenu)
         ([remap Info-search] . consult-info)
         ([remap repeat-complex-command] . consult-complex-command)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g I" . consult-imenu-multi)
         ("M-s d" . consult-find)
         ("M-s r" . consult-ripgrep)
         ("M-s G" . consult-git-grep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s m" . consult-man)
         ("M-s i" . consult-info)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (setq consult-narrow-key "<"
        consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
 --smart-case --no-heading --with-filename --line-number --search-zip\
 --hidden --glob !.git/* --glob !.direnv/*"))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init (vertico-mode))

(use-package embark
  :bind (("C-." . embark-act)
	 ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep :defer t)

(provide 'config-minibuffer)
