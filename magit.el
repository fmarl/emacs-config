(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'git-commit-setup-hook 'my/magit/add-jira-issue-to-commit-msg))

(defun my/magit/extract-jira-issue-from-branch ()
    (let ((branch-name (magit-get-current-branch)))
      (when (string-match "\\`\\([A-Z]+-[0-9]+\\)" branch-name)
	(match-string 0 branch-name))))

(defun my/magit/add-jira-issue-to-commit-msg ()
  (let ((jira-issue (my/magit/extract-jira-issue-from-branch)))
    (when jira-issue
	(insert (format "%s " jira-issue)))))
