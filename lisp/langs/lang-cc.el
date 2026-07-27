;;; lang-cc.el --- C/C++ development setup -*- lexical-binding: t; -*-

;; Kernel style including tabs-only argument lists, see
;; Documentation/process/coding-style.rst in the kernel tree
(defun my/c-lineup-arglist-tabs-only (_ignored)
  "Line up argument lists by tabs, not spaces."
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1) c-basic-offset)))

(use-package cc-mode
  :ensure nil
  :config
  (c-add-style "linux-tabs-only"
	       '("linux" (c-offsets-alist
			  (arglist-cont-nonempty
			   c-lineup-gcc-asm-reg
			   my/c-lineup-arglist-tabs-only))))
  :hook (((c-mode c++-mode) . eglot-ensure)
	 ((c-mode c++-mode) . (lambda ()
				(setq indent-tabs-mode t
				      tab-width 8
				      fill-column 80)
				(c-set-style "linux-tabs-only")))))

(provide 'lang-cc)
