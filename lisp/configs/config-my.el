;;; config-my.el --- Some custom functions -*- lexical-binding: t; -*-

(defconst my/kernel-src-dir (expand-file-name "~/src/kernel/linux/"))

(defun my/kernel-tree-p (&optional file)
  "Return non-nil if FILE (default `default-directory') is inside the kernel tree."
  (string-prefix-p my/kernel-src-dir (or file default-directory)))


(defun my/enable-lang (lang)
  (interactive
   (list (completing-read
	  "Lang: "
	  (mapcar (lambda (file) (substring (file-name-base file) 5))
		  (directory-files (expand-file-name "lisp/langs/" user-emacs-directory)
				   nil "\\`lang-.*\\.el\\'")))))
  (require (intern (concat "lang-" lang)))
  (revert-buffer-quick))

(defun my/update-config ()
  (interactive)
  (let ((init-file (expand-file-name "init.el" user-emacs-directory))
	(buffer (get-buffer-create "*git*")))
    (set-process-sentinel
     (start-process "git" buffer "git" "-C" user-emacs-directory "pull")
     (lambda (_proc event)
       (if (string= event "finished\n")
	   (load-file init-file)
	 (message "my/update-config: git pull failed (%s), see %s"
		  (string-trim event) (buffer-name buffer)))))))

(provide 'config-my)
