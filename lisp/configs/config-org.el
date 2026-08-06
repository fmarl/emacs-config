;;; config-org.el --- Modern Org-Mode Configuration -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . org-indent-mode)
         (org-mode . (lambda ()
                       (setq line-spacing 0.2)
                       (display-line-numbers-mode 0))))
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  (setq org-hide-emphasis-markers t
	org-pretty-entities t
	org-startup-folded 'content
	org-startup-with-inline-images t
	org-image-actual-width '(400)
	org-ellipsis " ▼ "
	org-log-done 'time
	org-log-into-drawer t
	org-return-follows-link t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-directory (expand-file-name "~/org/")
        org-agenda-files (list (expand-file-name "inbox.org" org-directory)
                               (expand-file-name "tasks.org" org-directory))
        org-default-notes-file (expand-file-name "inbox.org" org-directory))

  (setq org-capture-templates
        '(("t" "Task" entry (file "inbox.org")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:")
          ("n" "Note" entry (file "inbox.org")
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i")))

  (setq org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  (setq org-agenda-window-setup 'current-window
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-deadline-warning-days 7)

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)))
            (todo "IN-PROGRESS" ((org-agenda-overriding-header "In progress")))
            (tags-todo "-SCHEDULED={.+}-DEADLINE={.+}"
                       ((org-agenda-overriding-header "Unscheduled"))))))))


(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "●" "◆" "◇" "▶")
        org-modern-table t
        org-modern-checkbox '((?X . "☑") (?- . "☒") (?\s . "☐"))
        org-modern-todo-faces
        '(("TODO" . (:foreground "#BF616A" :weight bold))
          ("IN-PROGRESS" . (:foreground "#EBCB8B" :weight bold))
          ("DONE" . (:foreground "#A3BE8C" :weight bold))
          ("CANCELLED" . (:foreground "#5E81AC" :weight normal)))))

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n k" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (expand-file-name "~/org/denote/"))
  (denote-rename-buffer-mode 1))

(provide 'config-org)
