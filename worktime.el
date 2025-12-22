;; -*- lexical-binding: t; -*-

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

(defun worktime--start-work (hour minute)
  ())
