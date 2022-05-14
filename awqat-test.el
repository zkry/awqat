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
(require 'cl-lib)

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

(defmacro awqat--with-preset-fun (preset-fun lat-lon &rest body)
  (declare (indent defun))
  (let ((prayer-funs awqat--prayer-funs)
        (fajr-angle awqat-fajr-angle)
        (isha-angle awqat-isha-angle)
        (lat calendar-latitude)
        (lon calendar-longitude)
        (safety awqat-prayer-safety-offsets))
    `(let ((ret))
       (setq calendar-latitude ,(car lat-lon))
       (setq calendar-longitude ,(cadr lat-lon))
       (setq awqat-prayer-safety-offsets (make-list 6 0))
       (,preset-fun)
       (setq awqat-prayer-safety-offsets (quote ,safety))
       (setq ret (progn ,@body))
       (setq calendar-latitude ,lat)
       (setq calendar-longitude ,lon)
       ret)))

(defun str->date (ds)
  "Convert DD.MM.YYYY string DS to list of (MM DD YYYY)."
  (let ((parts (mapcar #'string-to-number (split-string ds "\\."))))
    (list (cl-second parts)
          (cl-first parts)
          (cl-third parts))))

(defun str->time (ts)
  "Convert HH:MM string TS to number of hours past 00:00."
  (let* ((parts (mapcar #'string-to-number (split-string ts ":")))
         (h (cl-first parts))
         (m (cl-second parts)))
    (+ h (/ m 60.0))))

(defun time->str (time)
  "Convert TIME in hours to string."
  (format "%d:%d" (floor time) (* (- time (floor time)) 60)))

(ert-deftest test-diyanet-times-berlin ()
  "Tests that the calculated times in Berlin match given times by Diyanet."
  (awqat--with-preset-fun awqat-set-preset-diyanet (52.439 13.4364)
    (dolist (test-case '(("17.02.2020" "05:25" "07:13" "12:26" "14:54" "17:28" "18:48")
                         ("20.02.2020" "05:19" "07:07" "12:25" "14:58" "17:33" "18:53")
                         ("13.03.2020" "04:34" "06:18" "12:21" "15:26" "18:14" "19:34")
                         ("15.03.2020" "04:32" "06:14" "12:20" "15:29" "18:17" "19:37")))
      (let* ((date (str->date (cl-first test-case)))
             (fajr (str->time (cl-second test-case)))
             (got-fajr (car (awqat--prayer-fajr date))))
        (message (format "diff= %s  (%s)" (abs (- fajr got-fajr)) (time->str got-fajr)))))))

(ert-deftest test-french-muslims-paris ()
  "Tests that the calculated times in Berlin match given times by Diyanet."
  (awqat--with-preset-fun awqat-set-preset-french-muslims (48.859 2.277)
    (dolist (test-case '(("11.05.2022" "04:49" "06:15" "13:47" "17:54" "09:21" "22:47")
                         ("14.05.2022" "04:43" "06:11" "13:47" "17:56" "09:25" "22:52")))
      (let* ((date (str->date (cl-first test-case)))
             (fajr (str->time (cl-second test-case)))
             (got-fajr (car (awqat--prayer-fajr date))))
        (message (format "diff= %s  (%s)" (abs (- fajr got-fajr)) (time->str got-fajr)))))))

(provide 'awqat-test)
;;; awqat-test.el ends here
