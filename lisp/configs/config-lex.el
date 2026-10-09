;;; config-lex.el --- Work-related config -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(dolist (dir (reverse
              (list (expand-file-name "~/.local/bin")
                    (concat "/etc/profiles/per-user/" (user-login-name) "/bin")
                    "/run/current-system/sw/bin"
                    "/nix/var/nix/profiles/default/bin"
                    "/opt/homebrew/bin")))
  (when (and (file-directory-p dir)
             (not (member dir exec-path)))
    (push dir exec-path)
    (setenv "PATH" (concat dir ":" (getenv "PATH")))))

;; Defined only in macOS builds; declared so this compiles everywhere.
(defvar mac-command-modifier)
(defvar mac-control-modifier)
(defvar mac-option-modifier)

(setq mac-command-modifier 'control
      mac-control-modifier 'super
      mac-option-modifier 'meta)

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
  "Insert the Jira issue key of the current branch into the commit message."
  (let ((jira-issue (my/magit/extract-jira-issue-from-branch)))
    (when jira-issue
      (insert (format "%s " jira-issue)))))

(add-hook 'git-commit-setup-hook #'my/magit/add-jira-issue-to-commit-msg)

(use-package worktime
  :load-path "lisp/worktime/"
  :config (worktime-mode))

(provide 'config-lex)
