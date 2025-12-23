;;; worktime.el --- Calculate the remaining time for the current working day -*- lexical-binding: t; -*-

;; Author: Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools
;; URL: https://github.com/fmarl/worktime.el

;;; Code:

(defgroup worktime nil
  "Calculate the remaining worktime in the mode-line"
  :group 'tools
  :prefix "worktime-")

(defcustom hours-per-week 40
  "How many hours per week you need to work"
  :type 'integer
  :group 'worktime)

(defcustom days-per-week 5
  "How many days do you need to work"
  :type 'integer
  :group 'worktime)

(defcustom break-per-day 30
  "How many minutes do you make break"
  :type 'integer
  :group 'worktime)

(defvar worktime--start-time nil)

(defvar worktime--end-time nil)

(defvar worktime--modeline-string "")

(defvar worktime--timer nil)

(defun worktime--start-work (hour minute)
  (interactive "nStart Hour: \nnStart Minute: ")
  (let* ((minutes-per-day (* 24 60))
	 (start-time (mod (+ (mod (+ (* hour 60) minute) minutes-per-day) minutes-per-day) minutes-per-day)))
    (progn
      (setq worktime--start-time start-time)
      (worktime--add (/ hours-per-week days-per-week) 0)
      (worktime--update))))

(defun worktime--add (hours minutes)
  (let ((minutes-per-day (* 24 60)))
    (if (not (null worktime--start-time))
	(setq worktime--end-time
	      (mod (+ worktime--start-time (+ (* hours 60) minutes break-per-day)) minutes-per-day))
      '())))

(defun worktime--to-string (sign time)
  (let ((hour (/ time 60))
	(minute (mod time 60)))
    (format "%s%02d:%02d" sign hour minute)))

(defun worktime--end-time-to-string ()
  (worktime--to-string "" worktime--end-time))

(defun worktime--current-minute-of-day ()
  (pcase-let ((`(_ ,min ,hour . ,_) (decode-time (current-time))))
    (+ (* hour 60) min)))

(defun worktime--remaining-to-string ()
  (let* ((remaining-time (- worktime--end-time (worktime--current-minute-of-day)))
	 (sign (if (> remaining-time 0) "-" "")))
    (worktime--to-string sign remaining-time)))

(defun worktime--update ()
  (if (not (null worktime--start-time))
      (let ((end-time (worktime--end-time-to-string))
	    (remaining-time (worktime--remaining-to-string)))
	(setq worktime--modeline-string
	      (format " %s %s" remaining-time end-time)))
    "")
  (force-mode-line-update t))

(defun worktime--start-timer ()
  (setq worktime--timer
	(run-at-time
	 (- 60 (nth 1 (decode-time (current-time))))
	 60
	 #'worktime--update)))

(defun worktime--stop-timer ()
  (when worktime--timer
    (cancel-timer worktime--timer)
    (setq worktime--timer nil)))
(defun worktime--register-modeline ()
  (add-to-list 'minor-mode-alist
	       '(worktime-mode (:eval worktime--modeline-string))))

(define-minor-mode worktime-mode
  "Minor mode which lets you track your worktime"
  :lighter nil
  :global t
  (if worktime-mode
      (progn
	(worktime--register-modeline)
	(worktime--start-timer)
	(worktime--update))
    (worktime--stop-timer)))

(provide 'worktime)
