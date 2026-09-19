;;; config-lex.el --- Work-related config -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Some MacOS compatibility stuff
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)
(setq mac-option-modifier 'meta)

;; Terraform
(use-package terraform-mode
  :mode "\\.tf\\'"
  :hook (terraform-mode . eglot-ensure))

(defun my/run-finalize ()
  "Run tools/finalize of the current project on this file."
  (interactive)
  (let ((vuln (read-string "Vulnerable?: "))
        (ignored (read-string "Ignore?: "))
        (days (read-string "Days?: "))
        (root (project-root (project-current t))))
    (compile (format "printf '%%s\\n' %s %s %s | %s -file %s"
                     (shell-quote-argument vuln)
                     (shell-quote-argument ignored)
                     (shell-quote-argument days)
                     (shell-quote-argument (expand-file-name "tools/finalize" root))
                     (shell-quote-argument (buffer-file-name))))))

(defun my/magit/extract-jira-issue-from-branch ()
  "Extract a Jira Issue Key from the current branch."
  (let ((branch-name (magit-get-current-branch)))
    (when (string-match "\\`\\([A-Z]+-[0-9]+\\)" branch-name)
      (match-string 0 branch-name))))

(defun my/magit/add-jira-issue-to-commit-msg ()
  "Extract a Jira Issue Key from the current branch and insert it into the commit msg."
  (let ((jira-issue (my/magit/extract-jira-issue-from-branch)))
    (when jira-issue
      (insert (format "%s " jira-issue)))))

(add-hook 'git-commit-setup-hook #'my/magit/add-jira-issue-to-commit-msg)

(use-package worktime
  :load-path "lisp/worktime/"
  :config (worktime-mode))

(provide 'config-lex)
