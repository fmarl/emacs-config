 ;;; config-magit.el --- Magit related config -*- lexical-binding: t; -*-

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'git-commit-setup-hook 'my/magit/add-jira-issue-to-commit-msg)
  (add-hook 'git-commit-post-finish-hook 'my/magit/hotfix-log-to-org))

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

(defun my/magit/hotfix-log-to-org ()
  "If the new commit contains the word 'Hotfix:', log it's content to a .org file."
  (let ((commit-msg (shell-command-to-string "git log -1 --pretty=%B")))
    (when (string-match-p "Hotfix:" commit-msg)
      (let* ((repo-name (file-name-nondirectory (directory-file-name (magit-toplevel))))
             (diff (shell-command-to-string "git show --pretty=medium HEAD"))
             (org-file (expand-file-name "~/Org/hotfix-log.org")))
        (with-temp-buffer
          (when (file-exists-p org-file)
            (insert-file-contents org-file))
          (goto-char (point-max))
          (insert (format "* Hotfix in %s\n" repo-name))
          (insert (format "Diff:\n#+BEGIN_SRC diff\n%s\n#+END_SRC\n\n" diff))
          (write-region (point-min) (point-max) org-file))))))

(provide 'config-magit)
