;;; config-org.el --- Modern Org-Mode Configuration -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'my-cache)

(defun my/org-mode-setup ()
  "Prose-friendly buffer settings for Org."
  (setq-local line-spacing 0.2))

(use-package org
  :hook ((org-mode . variable-pitch-mode)
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
  (org-directory "~/Documents/Org/")
  (org-agenda-files '("inbox.org" "tasks.org"))
  (org-id-locations-file (my/cache-file "org-id-locations"))
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
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-star 'replace)
  (org-modern-replace-stars '("◉" "○" "●" "◆" "◇" "▶"))
  (org-modern-table t)
  (org-modern-checkbox '((?X . "☑") (?- . "☒") (?\s . "☐")))
  (org-modern-todo-faces
   '(("TODO" . error)
     ("IN-PROGRESS" . warning)
     ("DONE" . success)
     ("CANCELLED" . shadow))))

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
  (denote-directory "~/Documents/Org/denote/")
  :config
  (denote-rename-buffer-mode))

(provide 'config-org)

;;; config-org.el ends here
