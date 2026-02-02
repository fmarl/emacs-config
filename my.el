(defun my/enable-lang (lang)
  (interactive "sLang: ")
  (progn
    (require (intern (concat "lang-" lang)))
    (revert-buffer-quick)))

(defun my/update-config ()
  (interactive)
  (let ((config-dir (expand-file-name "~/.emacs.d/"))
	(init-file (expand-file-name "~/.emacs.d/init.el"))
	(buffer (get-buffer-create "*git*")))
    (progn
      (start-process
       "git"
       buffer
       "git"
       "pull"
       config-dir)
      (load-file init-file))))
