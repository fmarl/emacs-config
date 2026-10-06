;;; config-org.el --- Modern Org-Mode Configuration -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(defun my/org-mode-setup ()
  "Prose-friendly buffer settings for Org."
  (setq line-spacing 0.2)
  (display-line-numbers-mode -1))

(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . org-indent-mode)
         (org-mode . my/org-mode-setup))
  :bind (("C-c a" . org-agenda)
         ("C-c o" . org-capture)
         ("C-c l" . org-store-link))
  :custom
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-startup-folded 'content)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(400))
  (org-ellipsis " ▼ ")
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-return-follows-link t)
  (org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-directory "~/org/")
  (org-agenda-files '("~/org/inbox.org" "~/org/tasks.org"))
  (org-default-notes-file "~/org/inbox.org")
  (org-capture-templates
   '(("t" "Task" entry (file "inbox.org")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:")
     ("n" "Note" entry (file "inbox.org")
      "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i")))
  (org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-agenda-window-setup 'current-window)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-deadline-warning-days 7)
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "" ((org-agenda-span 'day)))
       (todo "IN-PROGRESS" ((org-agenda-overriding-header "In progress")))
       (tags-todo "-SCHEDULED={.+}-DEADLINE={.+}"
                  ((org-agenda-overriding-header "Unscheduled"))))))))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star 'replace)
  (org-modern-replace-stars '("◉" "○" "●" "◆" "◇" "▶"))
  (org-modern-table t)
  (org-modern-checkbox '((?X . "☑") (?- . "☒") (?\s . "☐")))
  (org-modern-todo-faces
   '(("TODO" . (:foreground "#d67869" :weight bold))
     ("IN-PROGRESS" . (:foreground "#c09f6f" :weight bold))
     ("DONE" . (:foreground "#70bb70" :weight bold))
     ("CANCELLED" . (:foreground "#857f8f" :weight normal)))))

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n k" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :custom
  (denote-directory "~/org/denote/")
  :config
  (denote-rename-buffer-mode))

(provide 'config-org)
