;;; config-lex.el --- Work-related config -*- lexical-binding: t; -*-

;; Let Emacs see nix-darwin executables
(let* ((user (getenv "USER"))
       (nix-path (concat "/etc/profiles/per-user/" user "/bin")))
  (push nix-path exec-path)
  (setenv "PATH" (concat nix-path ":" (getenv "PATH"))))

;; Some MacOS compatibility stuff
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)
(setq mac-option-modifier 'meta)

;; Terraform
(use-package terraform-mode
  :mode "\\.tf\\'"
  :hook (terraform-mode . eglot-ensure))

;; YAML
(use-package yaml-mode :mode (("\\.yml\\'" . yaml-mode)
			      ("\\.yaml\\'" . yaml-mode)))

(defun my/run-finalize ()
  (interactive)
  (let* ((vuln (read-string "Vulnerable?: "))
         (ignore (read-string "Ignore?: "))
         (days (read-string "Days?: "))
         (cmd (format "printf '%s\n%s\n%s\n' | %s/tools/finalize -file %s"
                      vuln ignore days
		      (project-root (project-current buffer-file-name))
                      (shell-quote-argument (buffer-file-name)))))
    (compile cmd)))

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

(provide 'config-lex)
