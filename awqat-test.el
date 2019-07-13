;;; awqat-test.el --- Various tests for awqat

;; Copyright (C) 2019 Zachary Romero

;; Author: Zachary Romero <zacromero@posteo.net>
;; Url: http://github.com/zkry/awqat

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:



;;; Code:
(require 'awqat)

(defconst awqat--test-error 3.0
  "The window of error that time calculations should be in.")

(defun awqat--calculate-raw-times (date)
  "Calculate all of the times for diven day."
  (mapcar 'car (let ((times))
				 (dolist (f awqat-time-functions times)
				   (setq times (append times (list (apply f (list date)))))))))

(defun awqat--average-difference (lat lon preset &rest test-times)
  "Calculate times in TEST-TIMES at location LAT/LON with PRESET, return the average diff.

Test-TIMES is an cons cell with a list (MONTH DAY YEAR) in the
car cell and a list of the calculated times in the CDR cell.

PRESET is one of the presets defined in the awqat-use-preset
function used to set up calculation environment."
  (awqat-use-preset preset)
  (let* ((calendar-latitude lat)
		 (calendar-longitude lon)
		 (times-ct (length (car (cdadar test-times))))
		 (diffs (make-list times-ct 0))
		 (n 0))
	(dolist (date-times (car test-times))
	  (setq n (1+ n))
	  (let* ((date (car date-times))
			(expected-times (cadr date-times))
			(got-times (awqat--calculate-raw-times date)))
		(dotimes (i (length got-times))
		  (let ((diff (* 60 (abs (- (nth i expected-times)
									(nth i got-times)))))
				(diff-sum (nth i diffs)))
			(setcar (nthcdr i diffs) (+ diff-sum diff))))))
	(mapcar #'(lambda (x) (/ x n)) diffs)))

(defmacro ert-time-deftest (location preset &rest cases)
  "Define a test to control time quality given LOCATION, PRESET, and CASES.

LOCATION is a predefined location name that should resolve to a
latitude and longitude.  PRESET is the preset that will be used to
generate the times and CASES is a list of the test cases in the
form \"MM-DD-YYYY\" (\"HH:MM\" ...) \"MM-DD-YYYY\" (\"HH:MM\" ...)
 ..."
  ;; TODO: finish the list part of the dolist list
  (list 'ert-deftest (make-symbol
						(format "awqat-time-%s-%s"
								(replace-regexp-in-string ", " "-" (downcase location))
								(symbol-name preset)))
		'()
		(list 'dolist
			  (list 'err `(awqat--average-difference (car (awqat--lat-lon-from-loc ,location))
													 (cadr (awqat--lat-lon-from-loc ,location))
													 ',preset
													 (awqat--process-test-cases ',cases)))
			  `(should (< err awqat--test-error)))))

(defun awqat--parse-time-str (time)
  "Parse the TIME string to floating point number."
  (let* ((time-parts (split-string time ":"))
		 (hour (string-to-number (nth 0 time-parts)))
		 (minute (string-to-number (nth 1 time-parts))))
	(+ hour (/ minute 60.0))))

(defun awqat--parse-date-str (date)
  "Parse DATE string to '(MM DD YYYY)."
  (let ((date-parts (split-string date "-")))
	(mapcar #'string-to-number (list (nth 0 date-parts)
									 (nth 1 date-parts)
									 (nth 2 date-parts)))))

(defun awqat--process-test-cases (cases)
  "Process list of test CASES data to form usable by awqat--average-difference."
  (when (= 1 (mod (length cases) 2)) (error "Wrong number of test data items provided"))
  (let ((case-data '()))
	(dotimes (i (/ (length cases) 2) case-data)
	  (let ((date (awqat--parse-date-str (nth (* 2 i) cases)))
			(times (mapcar #'awqat--parse-time-str (nth (1+ (* 2 i)) cases))))
		(setq case-data (append case-data (list (list date times))))))))

(defun awqat--lat-lon-from-loc (loc)
  "Return a list of lat and lon from LOC."
  (cond ((equal loc "Oslo, Norway") '(59.9127 10.7461))
		(t (error "Unknown location"))))

;;; Test cases
(ert-time-deftest "Oslo, Norway" muslim-world-leauge
				  "1-1-2019" ("06:35" "09:18" "12:20" "13:16" "15:23" "17:58")
				  "1-2-2019" ("06:35" "09:17" "12:21" "13:17" "15:25" "17:59")
				  "7-1-2019" ("03:14" "03:58" "13:31" "19:20" "22:43" "23:27"))

(provide 'awqat-test)
;;; awqat-test.el ends here
