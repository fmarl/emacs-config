;;; worktime.el --- Calculate the remaining time for the current working day -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2026 Florian Marrero Liestmann
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Florian Marrero Liestmann <f.m.liestmann@fx-ttr.de>
;; Version: 0.2
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools
;; URL: https://github.com/fmarl/worktime.el

;;; Code:

(defgroup worktime nil
  "Show the remaining worktime in the mode line."
  :group 'tools
  :prefix "worktime-")

(defcustom worktime-hours-per-week 40
  "Hours of work per week."
  :type 'number
  :group 'worktime)

(defcustom worktime-days-per-week 5
  "Working days per week."
  :type 'integer
  :group 'worktime)

(defcustom worktime-break-per-day 30
  "Break per day in minutes."
  :type 'integer
  :group 'worktime)

(defconst worktime--minutes-per-day (* 24 60))

(defvar worktime--end-time nil
  "End of the working day in minutes since midnight, or nil.")

(defvar worktime--modeline-string "")

(defvar worktime--timer nil)

(defun worktime--minutes-per-workday ()
  "Minutes between start and end of a working day, including the break."
  (+ (round (/ (* worktime-hours-per-week 60.0) worktime-days-per-week))
     worktime-break-per-day))

(defun worktime--current-minute-of-day ()
  "Minutes since midnight."
  (pcase-let ((`(_ ,minute ,hour . ,_) (decode-time)))
    (+ (* hour 60) minute)))

(defun worktime--hhmm (minutes)
  "Format MINUTES as HH:MM."
  (format "%02d:%02d" (/ minutes 60) (mod minutes 60)))

(defun worktime--remaining-string ()
  "Remaining time as -HH:MM, or +HH:MM once the day is over."
  (let ((remaining (- worktime--end-time (worktime--current-minute-of-day))))
    (concat (if (> remaining 0) "-" "+") (worktime--hhmm (abs remaining)))))

(defun worktime--update ()
  "Refresh the mode line string."
  (setq worktime--modeline-string
        (if worktime--end-time
            (format " %s %s" (worktime--remaining-string) (worktime--hhmm worktime--end-time))
          ""))
  (force-mode-line-update t))

(defun worktime--start-timer ()
  "Update once per minute, aligned to the next full minute."
  (setq worktime--timer
        (run-at-time (- 60 (nth 0 (decode-time))) 60 #'worktime--update)))

(defun worktime--stop-timer ()
  "Cancel the update timer."
  (when worktime--timer
    (cancel-timer worktime--timer)
    (setq worktime--timer nil)))

;;;###autoload
(define-minor-mode worktime-mode
  "Show the remaining worktime in the mode line."
  :lighter (:eval worktime--modeline-string)
  :global t
  (worktime--stop-timer)
  (when worktime-mode
    (worktime--start-timer)
    (worktime--update)))

;;;###autoload
(defun worktime-start (hour minute)
  "Start the working day at HOUR:MINUTE and enable `worktime-mode'."
  (interactive "nStart hour: \nnStart minute: ")
  (setq worktime--end-time
        (mod (+ (* hour 60) minute (worktime--minutes-per-workday))
             worktime--minutes-per-day))
  (worktime-mode 1))

(provide 'worktime)
;;; worktime.el ends here
