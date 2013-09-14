;;; cl-date-time.lisp
;;; package "cl-date/time"
;;; quickly manage date-time and timestamp units
;;; written open-source for fair use by -nydel-

(defpackage :cl-date/time
  (:nicknames :cl-date-time :cl-date :date-time :cl-dt :dt)
  (:use :common-lisp)
  (:export :epoch-time
	   :new-cltime
	   :cltime
	   :cltime-decoded :cltime/decoded
	   :cltime-epoch   :cltime/epoch
	   :cltime-second  :cltime/second
	   :cltime-minute  :cltime/minute
	   :cltime-hour    :cltime/hour
	   :cltime-date    :cltime/date
	   :cltime-month   :cltime/month
	   :cltime-year    :cltime/year
	   :cltime-weekday :cltime/weekday
	   :cltime-dst-p   :cltime/dst-p
	   :cltime-tz      :cltime/tz
	   :replace-all
	   :time-string))

(in-package :cl-date/time)

(defstruct cltime
  "the simple cltime structure is this utility's heart"
  decoded epoch second minute hour date month year weekday dst-p tz)

(defparameter weekday-alist
  (pairlis '(0 1 2 3 4 5 6)
	   '("monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday")))

(defun epoch-time (&optional encoded-time)
  "returns timestamp standardized to unix/linux epoch, 1970-01-01"
  (let ((epoch-time (encode-universal-time 0 0 0 1 1 1970 0)))
    (if encoded-time
	(- encoded-time epoch-time)
	(- (get-universal-time) epoch-time))))

(defun new-cltime (&optional encoded-time)
  "central function returns a cltime structure"
  (let* ((decoded-time (if encoded-time
			  encoded-time
			  (get-universal-time)))
	 (data-list (multiple-value-list
		     (decode-universal-time decoded-time))))
    (make-cltime :decoded decoded-time
		 :epoch (epoch-time decoded-time)
		 :second (first data-list)
		 :minute (second data-list)
		 :hour (third data-list)
		 :date (fourth data-list)
		 :month (fifth data-list)
		 :year (sixth data-list)
		 :weekday (cdr (assoc (seventh data-list) weekday-alist))
		 :dst-p (eighth data-list)
		 :tz (ninth data-list))))

;;; shortcut functions to quickly pull individual date/time units

(defun cltime/decoded (&optional encoded-time)
  (cltime-decoded (new-cltime (if encoded-time encoded-time nil))))

(defun cltime/epoch (&optional encoded-time)
  (cltime-epoch (new-cltime (if encoded-time encoded-time nil))))

(defun cltime/second (&optional encoded-time)
  (cltime-second (new-cltime (if encoded-time encoded-time nil))))

(defun cltime/minute (&optional encoded-time)
  (cltime-minute (new-cltime (if encoded-time encoded-time nil))))

(defun cltime/hour (&optional encoded-time)
  (cltime-hour (new-cltime (if encoded-time encoded-time nil))))

(defun cltime/date (&optional encoded-time)
  (cltime-date (new-cltime (if encoded-time encoded-time nil))))

(defun cltime/month (&optional encoded-time)
  (cltime-month (new-cltime (if encoded-time encoded-time nil))))

(defun cltime/year (&optional encoded-time)
  (cltime-year (new-cltime (if encoded-time encoded-time nil))))

(defun cltime/weekday (&optional encoded-time)
  (cltime-weekday (new-cltime (if encoded-time encoded-time nil))))

(defun cltime/dst-p (&optional encoded-time)
  (cltime-dst-p (new-cltime (if encoded-time encoded-time nil))))

(defun cltime/tz (&optional encoded-time)
  (cltime-tz (new-cltime (if encoded-time encoded-time nil))))

;;; experimental string functions

(defun replace-all (string part replacement &key (test #'char=))
  "standard replace-all function"
  (with-output-to-string (out)
    (loop with part-length = (length part)
	 for old-pos = 0 then (+ pos part-length)
	 for pos = (search part string
			   :start2 old-pos
			   :test test)
	 do (write-string string out
			  :start old-pos
			  :end (or pos (length string)))
	 when pos do (write-string replacement out)
	 while pos)))

(defun time-string (string) ;;mediocre coding needs rewrite
  "formatting of date-time information into strings"
  (let ((str-result string)
	(unit-alist '(("%s" . (cltime/second))
		      ("%m" . (cltime/minute))
		      ("%h" . (cltime/hour))
		      ("%d" . (cltime/date))
		      ("%M" . (cltime/month))
		      ("%y" . (cltime/year))
		      ("%D" . (cltime/weekday)))))
    (loop for i in unit-alist do
	 (let ((unit (write-to-string (eval (cdr i)))))
	   (when (eq (length unit) 1)
	       (setf unit (concatenate 'string "0" unit)))
	   (setf str-result
		 (replace-all str-result
			      (car i)
			      unit))))
    (remove #\" str-result)))
