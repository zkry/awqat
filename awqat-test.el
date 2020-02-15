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

;;; lat-lon for Berlin
;; (setq calendar-latitude 52.439)
;; (setq calendar-longitude 13.4364)

;; ;; oslo
;; (setq calendar-latitude 59.9127)
;; (setq calendar-longitude 10.7461)

;; ;; istanbul
;; (setq calendar-latitude 40.94832)
;; (setq calendar-longitude 29.161208)

;; ;; beypazari ankara
;; (setq calendar-latitude 40.160273)
;; (setq calendar-longitude 31.921274)

;; ;; copenhagen
;; (setq calendar-latitude 55.66666)
;; (setq calendar-longitude 12.55407)

(defconst awqat--test-error 3.0
  "The window of error that time calculations should be in.")

(defun awqat--calculate-raw-times (date)
  "Calculate all of the times for diven day."
  (mapcar 'car (let ((times))
				 (dolist (f awqat-time-functions times)
				   (setq times (append times (list (apply f (list date)))))))))

(defun awqat--average-difference (offset lat lon preset &rest test-times)
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
									(+ offset
									   (nth i got-times))))))
				(diff-sum (nth i diffs)))
			(setcar (nthcdr i diffs) (+ diff-sum diff))))))
	(mapcar #'(lambda (x) (/ x n)) diffs)))

(defmacro ert-time-deftest (location preset time-zone &rest cases)
  "Define a test to control time quality given LOCATION, PRESET, and CASES.

LOCATION is a predefined location name that should resolve to a
latitude and longitude.  PRESET is the preset that will be used to
generate the times and CASES is a list of the test cases in the
form \"MM-DD-YYYY\" (\"HH:MM\" ...) \"MM-DD-YYYY\" (\"HH:MM\" ...)
 ..."
  ;; TODO: finish the list part of the dolist list
  (list 'let `((time-diffs (awqat--average-difference (awqat--diff-to-time-zone ,time-zone)
							(car (awqat--lat-lon-from-loc ,location))
													  (cadr (awqat--lat-lon-from-loc ,location))
													  ',preset
													  (awqat--process-test-cases ',cases))))
		(list 'dolist
			  (list 'err 'time-diffs)
			  (list 'message `(format "%s> %s" ,location err))
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
		((equal loc "Berlin, Germany") '(52.514460 13.412643))
		((equal loc "Istanbul, Turkey") '(40.94832 29.161208))
		((equal loc "Beypazarı, Turkey") '(40.160273 31.921274))
		(t (error "Unknown location"))))

(defun awqat--diff-to-time-zone (zone)
  "Return the difference of hours from current time zone to ZONE."
  (let ((offset (car (current-time-zone)))
		(zone-offset (cond ((equal zone "CEST") 7200)
						   ((equal zone "BST") 3600)
						   ((equal zone "TRT") 10800))))
	(/ (- zone-offset offset) 3600)))

;; Test cases
(ert-deftest awqat--time-accuracy ()
  "Test the generated times to ensure accuracy."
  ;; Taken from https://www.muslimpro.com/en/search?coordinates=59.9138688%2C10.752245399999993&country_code=NO&country_name=Norway&city_name=Oslo&date=&convention=MWL
  (ert-time-deftest "Beypazarı, Turkey" diyanet "TRT"
					"07-13-2019" ("03:35" "05:27" "13:03" "17:00" "20:29" "22:13")
					"08-01-2019" ("04:00" "05:43" "13:04" "16:57" "20:15" "21:50")
					"08-12-2019" ("04:16" "05:53" "13:03" "16:52" "20:02" "21:32"))
  (ert-time-deftest "Istanbul, Turkey" diyanet "TRT"
					"07-13-2019" ("03:41" "05:36" "13:14" "17:13" "20:43" "22:29")
					"07-19-2019" ("03:49" "05:40" "13:15" "17:12" "20:39" "22:23")
					"08-01-2019" ("04:08" "05:52" "13:15" "17:09" "20:28" "22:05")
					"08-06-2019" ("04:15" "05:57" "13:14" "17:07" "20:22" "21:57")
					"08-12-2019" ("04:24" "06:03" "13:14" "17:04" "20:15" "21:46"))
  (ert-time-deftest "Oslo, Norway" muslim-world-leauge "CEST"
					"01-01-2019" ("06:35" "09:18" "12:20" "13:16" "15:23" "17:58")
					"01-02-2019" ("06:35" "09:17" "12:21" "13:17" "15:25" "17:59")
					"03-01-2019" ("04:57" "07:15" "12:29" "14:59" "17:44" "19:53")
					"03-15-2019" ("04:12" "06:33" "12:26" "15:22" "18:18" "20:31")
					"03-20-2019" ("03:54" "06:18" "12:25" "15:30" "18:31" "20:46")
					"07-01-2019" ("02:24" "03:59" "13:21" "18:01" "22:43" "24:12")
					"07-15-2019" ("02:34" "04:20" "13:23" "17:58" "22:25" "24:06")
					"08-01-2019" ("02:49" "04:57" "13:23" "17:45" "21:49" "23:51")
					"09-01-2019" ("03:15" "06:11" "13:17" "17:00" "20:23" "23:06")
					"09-15-2019" ("04:12" "06:44" "13:12" "16:32" "19:41" "22:02")
					"10-01-2019" ("05:01" "07:21" "13:07" "15:58" "18:52" "21:04")
					"11-01-2019" ("05:16" "07:38" "12:01" "13:52" "16:23" "18:37")
					"12-21-2019" ("06:32" "09:17" "12:15" "13:07" "15:13" "17:49")))

(defun awqat--format-times ()
  "Format copied times to elisp syntax."
  (interactive)
  (beginning-of-line)
  (search-forward-regexp "(")
  (dotimes (i 6)
	(insert "\"")
	(while (looking-at "[0-9:]")
	  (forward-char))
	(insert "\"")
	(while (looking-at "[[:blank:]]")
	  (delete-char 1))
	(when (not (= i 5)) (insert " "))))

(provide 'awqat-test)
;;; awqat-test.el ends here
