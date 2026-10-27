;;; build.el --- Format, check and compile the configuration -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'bytecomp)

(defvar build--entry-files '("early-init.el" "init.el"))

(defvar build--modules (directory-files-recursively "lisp" "\\.el\\'")
  "Elisp files under lisp/, relative to the repository root.")

(defvar build--expected-modes
  '(savehist-mode recentf-mode save-place-mode global-auto-revert-mode
    vertico-mode marginalia-mode global-corfu-mode which-key-mode)
  "Global modes that must be enabled after loading the configuration.")

(defun build--activate ()
  (setq user-emacs-directory default-directory)
  (dolist (dir (seq-uniq (mapcar #'file-name-directory build--modules)))
    (add-to-list 'load-path (expand-file-name dir)))
  (package-activate-all))

(defun build--load-init ()
  "Load the configuration the way Emacs does at startup."
  (setq user-emacs-directory default-directory)
  (load (expand-file-name "early-init.el"))
  (package-activate-all)
  (let ((debug-on-error t))
    (load (expand-file-name "init.el"))))

(defun build--used-modules ()
  (seq-filter (lambda (file) (featurep (intern (file-name-base file))))
              build--modules))

(defun build--check-in-fresh-emacs (file)
  (with-temp-buffer
    (let ((status (call-process (expand-file-name invocation-name
                                                  invocation-directory)
                                nil t nil
                                "--batch" "-l" (expand-file-name "scripts/build.el")
                                "-f" "build-check-file" file)))
      (unless (bobp)
        (message "%s" (string-trim-right (buffer-string))))
      (eql status 0))))

(defun build--mode-enabled-p (mode)
  (or (and (boundp mode) (symbol-value mode))
      (progn (message "init.el: `%s' is not enabled" mode) nil)))

(defun build-fmt ()
  "Reindent every file and strip trailing whitespace."
  (setq-default indent-tabs-mode nil)
  (dolist (file (append build--entry-files build--modules))
    (with-current-buffer (find-file-noselect file)
      (indent-region (point-min) (point-max))
      (delete-trailing-whitespace)
      (when (buffer-modified-p)
        (save-buffer)
        (message "formatted %s" file)))))

(defun build-check ()
  "Load the configuration, then byte-compile each file in a fresh Emacs."
  (build--load-init)
  (let ((modes-ok (seq-every-p #'build--mode-enabled-p build--expected-modes))
        (failed (or (seq-remove #'build--check-in-fresh-emacs
                                (build--used-modules))
                    (seq-remove #'build--check-in-fresh-emacs
                                build--entry-files))))
    (kill-emacs (if (and modes-ok (not failed)) 0 1))))

(defun build-check-file ()
  "Byte-compile the file named on the command line, warnings as errors."
  (build--activate)
  (let* ((file (pop command-line-args-left))
         (dir (make-temp-file "emacs-check-" t))
         (byte-compile-error-on-warn t)
         (byte-compile-dest-file-function
          (lambda (src)
            (expand-file-name (concat (file-name-nondirectory src) "c") dir)))
         (missing nil)
         ;; Otherwise use-package only reports missing packages
         (debug-on-error '(file-missing))
         (debugger (lambda (_kind data &rest _) (push data missing) nil))
         (compiled (unwind-protect
                       (byte-compile-file file)
                     (delete-directory dir t))))
    (dolist (err (reverse missing))
      (message "%s: %s" file (error-message-string err)))
    (kill-emacs (if (and compiled (not missing)) 0 1))))

(defun build-compile ()
  "Byte-compile the used modules next to their sources, then native-compile."
  (build--load-init)
  (let* ((modules (build--used-modules))
         (failed (seq-remove #'byte-compile-file modules)))
    (when (native-comp-available-p)
      (mapc #'native-compile (seq-difference modules failed)))
    (kill-emacs (if failed 1 0))))

;;; build.el ends here
