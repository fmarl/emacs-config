;;; config-my.el --- Some custom functions -*- lexical-binding: t; -*-

(defun my/enable-lang (lang)
  (interactive "sLang: ")
  (progn
    (require (intern (concat "lang-" lang)))
    (revert-buffer-quick)))

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
