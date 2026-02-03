;;; config-lex.el --- Work-related config -*- lexical-binding: t; -*-

;; Let Emacs see nix-darwin executables
(let* ((user (getenv "USER"))
       (nix-path (concat "/etc/profiles/per-user/" user "/bin")))
  (push nix-path exec-path)
  (setenv "PATH" (concat nix-path ":" (getenv "PATH"))))

;; Some MacOS compatibility stuff
(setq ring-bell-function 'ignore)
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)
(setq mac-option-modifier 'meta)

;; Terraform
(use-package terraform-mode :mode "\\.tf\\'")

;; YAML
(use-package yaml-mode :mode (("\\.yml\\'" . yaml-mode)
			      ("\\.yaml\\'" . yaml-mode)))

(defun run-git-shepherd ()
  "Run git-shepherd to sync local git repos"
  (interactive)
  (let ((shepherd (expand-file-name "~/Devel/SecEn/git-shepherd/main.py"))
	(buffer (get-buffer-create "*git-shepherd*")))
    (start-process
     "git-shepherd"
     buffer
     "python"
     shepherd)
    (display-buffer buffer)))

(defun edit-git-shepherd ()
  (interactive)
  (find-file-other-window (expand-file-name "~/.git-shepherd")))

(use-package worktime
  :load-path "lisp/worktime/"
  :config (worktime-mode))

(use-package inheritenv)

(use-package vterm)

(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :bind-keymap ("C-x c" . claude-code-command-map)
  :config
  (claude-code-mode)
  (setq claude-code-terminal-backend 'vterm))

(provide 'config-lex)
