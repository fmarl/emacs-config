 ;;; config-completion.el --- Completion-frontend config -*- lexical-binding: t; -*-

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt minibuffer-visible-completions t)              ; Use ↑↓ to select candidates
(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is an arbitrary value
(setopt completions-format 'one-column)                ; Makes it easier to scroll
(setopt completions-group t)

;; Eager completion setup: show *Completions* buffer immediately
(setopt completion-auto-select 'second-tab)            ; Much more eager
(setopt completion-eager-display t)                    ; Show the completions buffer immediately
(setopt completion-eager-update t)                     ; Update display as-you-type


(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-preview-current nil))

(use-package cape
  :after corfu
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package yasnippet-snippets :after yasnippet)

(provide 'config-completion)
