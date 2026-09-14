;;; build.el --- Format, check and compile the configuration -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

(defconst build-modules (directory-files-recursively "lisp" "\\.el\\'")
  "Elisp files under lisp/, relative to the repository root.")

(defconst build-files (append '("early-init.el" "init.el") build-modules)
  "Every Elisp file of the configuration.")

(defun build--load-init ()
  "Load the configuration the way Emacs does at startup."
  (setq user-emacs-directory default-directory)
  (load (expand-file-name "early-init.el"))
  (package-activate-all)
  (let ((debug-on-error t))
    (load (expand-file-name "init.el"))))

(defun build-fmt ()
  "Reindent every file with spaces and strip trailing whitespace."
  (setq-default indent-tabs-mode nil)
  (dolist (file build-files)
    (with-current-buffer (find-file-noselect file)
      (untabify (point-min) (point-max))
      (indent-region (point-min) (point-max))
      (delete-trailing-whitespace)
      (when (buffer-modified-p)
        (save-buffer)
        (message "formatted %s" file)))))

(defun build-check ()
  "Load the configuration, then byte-compile every file with warnings as errors."
  (build--load-init)
  (let* ((dir (make-temp-file "emacs-check-" t))
         (byte-compile-error-on-warn t)
         (byte-compile-dest-file-function
          (lambda (file)
            (expand-file-name (concat (file-name-nondirectory file) "c") dir)))
         (failed (unwind-protect
                     (seq-remove #'byte-compile-file build-files)
                   (delete-directory dir t))))
    (kill-emacs (if failed 1 0))))

(defun build-compile ()
  "Byte-compile the modules next to their sources and native-compile them."
  (build--load-init)
  (let ((failed (seq-remove #'byte-compile-file build-modules)))
    (when (native-comp-available-p)
      (mapc #'native-compile (seq-difference build-modules failed)))
    (kill-emacs (if failed 1 0))))

;;; build.el ends here
