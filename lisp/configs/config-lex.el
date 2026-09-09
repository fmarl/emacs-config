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

;; ACP agents: pi (-> LM Studio, config in ~/.pi/agent/) runs on the host;
;; Claude Code runs inside the project's lx-claude-devcontainer, hence the
;; claude-code-scoped command prefix and the /workspace path mapping below.

(defun my/lx-claude--host-root ()
  "Project root on the host, sans trailing slash."
  (directory-file-name
   (expand-file-name
    (if-let* ((project (project-current)))
	(project-root project)
      default-directory))))

(defun my/lx-claude--container ()
  "Container name of the current project's devcontainer.
The launcher names containers lx-claude-devcontainer-<workspace-basename>."
  (concat "lx-claude-devcontainer-"
	  (file-name-nondirectory (my/lx-claude--host-root))))

(defun my/lx-claude-container-prefix (buffer)
  "Run Claude Code agent/shell commands inside the project's devcontainer.
Other agents (pi) return nil and stay on the host."
  (when (eq (map-elt (agent-shell-get-config buffer) :identifier) 'claude-code)
    (with-current-buffer buffer
      (list "docker" "exec" "-i" "--user" "node" "-w" "/workspace"
	    (my/lx-claude--container)))))

(defun my/lx-claude-resolve-path (path)
  "Map paths between the host project root and the /workspace bind mount.
Only for Claude Code sessions; pi paths pass through untouched."
  (let ((config (ignore-errors (agent-shell-get-config (current-buffer)))))
    (if (and path (eq (map-elt config :identifier) 'claude-code))
	(let ((root (my/lx-claude--host-root))
	      (path (expand-file-name path)))
	  (cond ((equal path "/workspace") root)
		((string-prefix-p "/workspace/" path)
		 (concat root (substring path (length "/workspace"))))
		((equal path root) "/workspace")
		((string-prefix-p (concat root "/") path)
		 (concat "/workspace" (substring path (length root))))
		(t path)))
      path)))

(defun my/lx-claude--repo-targets ()
  "The /repos[N] paths currently present in the running devcontainer.
Signals `user-error' when the container is not running."
  (with-temp-buffer
    (unless (zerop (call-process "docker" nil '(t nil) nil
				 "exec" (my/lx-claude--container)
				 "sh" "-c" "ls -d /repos* 2>/dev/null; exit 0"))
      (user-error "Container %s is not running" (my/lx-claude--container)))
    (split-string (buffer-string) "\n" t)))

(defun my/lx-claude-add-repo (dir)
  "Expose DIR read-only in the running devcontainer at the next free /repos[N].
Docker cannot attach bind mounts to a running container, so this copies a
point-in-time snapshot (docker cp), root-owned and write-protected to mirror
the launcher's -a mounts.  To pick up host-side changes, remove the snapshot
via `my/lx-claude-remove-repo' and add it again."
  (interactive "DAdd repo to container: ")
  (let* ((container (my/lx-claude--container))
	 (src (directory-file-name (expand-file-name dir)))
	 (taken (my/lx-claude--repo-targets))
	 (n 1)
	 (target "/repos"))
    (while (member target taken)
      (setq n (1+ n)
	    target (format "/repos%d" n)))
    (let ((process
	   (start-process-shell-command
	    "lx-claude-add-repo" (get-buffer-create "*lx-claude-repos*")
	    (format "docker cp %s %s:%s && docker exec -u root %s sh -c %s"
		    (shell-quote-argument src)
		    (shell-quote-argument container) target
		    (shell-quote-argument container)
		    (shell-quote-argument
		     (format "chown -R root:root %s && chmod -R a-w %s" target target))))))
      (set-process-sentinel process
			    (lambda (_ event)
			      (message "add-repo %s -> %s: %s" src target (string-trim event))))
      (message "Copying %s -> %s:%s ..." src container target))))

(defun my/lx-claude-remove-repo (target)
  "Delete a copied /repos[N] snapshot from the running devcontainer.
Launch-time -a bind mounts survive this: they are mounted read-only, so the
in-container rm fails on them."
  (interactive
   (let ((targets (my/lx-claude--repo-targets)))
     (unless targets
       (user-error "Nothing under /repos* in %s" (my/lx-claude--container)))
     (list (completing-read "Remove from container: " targets nil t))))
  (unless (string-match-p "\\`/repos[0-9]*\\'" target)
    (user-error "Refusing to delete %s" target))
  (let ((process (start-process "lx-claude-remove-repo" "*lx-claude-repos*"
				"docker" "exec" "-u" "root" (my/lx-claude--container)
				"rm" "-rf" "--one-file-system" target)))
    (set-process-sentinel process
			  (lambda (_ event)
			    (message "remove-repo %s: %s" target (string-trim event))))))

(use-package agent-shell
  :config
  (setq agent-shell-command-prefix #'my/lx-claude-container-prefix)
  (setq agent-shell-path-resolver-function #'my/lx-claude-resolve-path))

(provide 'config-lex)
