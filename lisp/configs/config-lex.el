;;; config-lex.el --- Work-related config -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(let* ((dirs (seq-filter #'file-directory-p
                         (list (expand-file-name "~/.local/bin")
                               (file-name-concat "/etc/profiles/per-user"
                                                 (user-login-name) "bin")
                               "/run/current-system/sw/bin"
                               "/nix/var/nix/profiles/default/bin"
                               "/opt/homebrew/bin")))
       (path (seq-uniq (append dirs (split-string (getenv "PATH")
                                                  path-separator t)))))
  (setenv "PATH" (string-join path path-separator))
  (setq exec-path (append path (list exec-directory))))

(defvar mac-command-modifier)
(defvar mac-control-modifier)
(defvar mac-option-modifier)

(setq mac-command-modifier 'control
      mac-control-modifier 'super
      mac-option-modifier 'meta)

(use-package terraform-mode
  :mode "\\.tf\\'"
  :hook (terraform-mode . eglot-ensure))

;; apheleia defaults terraform-mode to opentofu
(defvar apheleia-mode-alist)
(with-eval-after-load 'apheleia
  (setf (alist-get 'terraform-mode apheleia-mode-alist) 'terraform))

(defun my/run-finalize ()
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (let ((vuln (read-string "Vulnerable?: "))
        (ignored (read-string "Ignore?: "))
        (days (read-string "Days?: "))
        (root (project-root (project-current t))))
    (compile (format "printf '%%s\\n' %s %s %s | %s -file %s"
                     (shell-quote-argument vuln)
                     (shell-quote-argument ignored)
                     (shell-quote-argument days)
                     (shell-quote-argument (expand-file-name "tools/finalize" root))
                     (shell-quote-argument buffer-file-name)))))

(declare-function magit-get-current-branch "magit-git")

(defun my/current-jira-issue ()
  (when-let* ((branch (magit-get-current-branch))
              ((let ((case-fold-search nil))
                 (string-match "\\`[A-Z]+-[0-9]+" branch))))
    (match-string 0 branch)))

(defun my/git-commit-insert-jira-issue ()
  (when-let* (((looking-at-p "$"))
              (issue (my/current-jira-issue)))
    (insert issue " ")))

(add-hook 'git-commit-setup-hook #'my/git-commit-insert-jira-issue)

(use-package worktime
  :load-path "lisp/worktime/"
  :functions worktime-mode
  :config (worktime-mode))

(provide 'config-lex)

;;; config-lex.el ends here
