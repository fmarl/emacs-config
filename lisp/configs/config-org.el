;;; config-org.el --- Modern Org-Mode Configuration -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :hook ((org-mode . visual-line-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . org-indent-mode)
         (org-mode . (lambda ()
                       (setq line-spacing 0.2)
                       (display-line-numbers-mode 0))))
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

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(provide 'config-org)
