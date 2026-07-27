;;; config-my.el --- Some custom functions -*- lexical-binding: t; -*-

(defconst my/kernel-src-dir (expand-file-name "~/src/kernel/linux/"))

(defun my/enable-lang (lang)
  (interactive
   (list (completing-read
	  "Lang: "
	  (mapcar (lambda (file) (substring (file-name-base file) 5))
		  (directory-files (expand-file-name "lisp/langs/" user-emacs-directory)
				   nil "\\`lang-.*\\.el\\'")))))
  (require (intern (concat "lang-" lang)))
  (revert-buffer-quick))

(defconst my/lang-mode-alist
  '((c-mode . "cc") (c++-mode . "cc")
    (clojure-mode . "clojure")
    (elixir-ts-mode . "elixir") (heex-ts-mode . "elixir")
    (gleam-ts-mode . "gleam")
    (go-ts-mode . "go") (go-mod-ts-mode . "go")
    (haskell-mode . "haskell")
    (java-mode . "java") (java-ts-mode . "java")
    (nasm-mode . "nasm")
    (nix-ts-mode . "nix")
    (tuareg-mode . "ocaml")
    (python-mode . "python") (python-ts-mode . "python")
    (rust-mode . "rust") (rustic-mode . "rust")
    (sh-mode . "shell") (bash-ts-mode . "shell")
    (zig-mode . "zig"))
  "Major modes and the lang module that configures them.")

(defun my/lang-auto-enable ()
  "Load the lang module for the current major mode on first use."
  (when-let* ((lang (alist-get major-mode my/lang-mode-alist))
	      (feature (intern (concat "lang-" lang))))
    (unless (featurep feature)
      (require feature)
      ;; re-run mode selection so hooks and remaps from the
      ;; freshly loaded module apply to this buffer as well
      (when buffer-file-name
	(normal-mode)))))

(add-hook 'after-change-major-mode-hook #'my/lang-auto-enable)

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
