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
  (interactive)
  (let* ((vuln (read-string "Vulnerable?: "))
         (ignore (read-string "Ignore?: "))
         (days (read-string "Days?: "))
         (cmd (format "printf '%s\n%s\n%s\n' | %s/tools/finalize -file %s"
                      vuln ignore days
                      (project-root (project-current buffer-file-name))
                      (shell-quote-argument (buffer-file-name)))))
    (compile cmd)))

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
