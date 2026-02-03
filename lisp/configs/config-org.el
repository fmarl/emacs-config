;;; config-org.el --- Modern Org-Mode Configuration -*- lexical-binding: t; -*-

(require 'org)

;; Basic Org Setup
(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . org-indent-mode)
         (org-mode . (lambda ()
                       (setq line-spacing 0.2)
                       (display-line-numbers-mode 0))))
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-hide-emphasis-markers t
	org-pretty-entities t
	org-startup-indented t
	org-startup-folded 'content
	org-startup-with-inline-images t
	org-image-actual-width '(400)
	org-ellipsis " ▼ "
	org-log-done 'time
	org-log-into-drawer t
	org-return-follows-link t)

  ;; Verb
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  
  ;; Todo keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)"))))


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
  (setq denote-directory (expand-file-name "~/Org/denote/"))
  (denote-rename-buffer-mode 1))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Org/roam/"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(defun my/org-new-dated-file ()
  "Create a new org-mode buffer with the current date as name."
  (interactive)
  (let ((buffer-name (format "%s.org" (format-time-string "%Y-%m-%d"))))
    (switch-to-buffer (get-buffer-create buffer-name))
    (org-mode)
    (insert (format "#+TITLE: %s\n\n* TODO \n" (format-time-string "%A, %d %B %Y")))))


(provide 'config-org)
