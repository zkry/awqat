;;; awqat.el --- Calculations of positions of the sun -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Zachary Romero

;; Author: Zachary Romero <zacromero@posteo.net>
;; Url: http://github.com/zkry/

;; Special thanks to the authors of the built in solar.el library.

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

(require 'solar)
(require 'calendar)

;;; lat-lon for Berlin
(setq calendar-latitude 52.4939)
(setq calendar-longitude 13.4364)


;; NOTE: DATE = '(M D Y)

(defvar awqat-time-functions '()
  "Stores a list of the time functions to use.")
(setq awqat-time-functions (list 'awqat-fajr 'awqat-imsak))
;;; General settings
(defvar awqat-sunrise-sunset-below-angle -1.66)

;;; Islamic prayer times settings
(defvar awqat-fajr-angle -18.0)
(defvar awqat-isha-angle -17.0)

;;; Application functions
(defun awqat-times-for-day ()
  "Calculate all of the times for current day."
  (interactive)
  (let ((date (list
			   (string-to-number (format-time-string "%m"))
			   (string-to-number (format-time-string "%d"))
			   (string-to-number (format-time-string "%Y"))))
		(times))
	(message (format
			  "%s"
			  (dolist (f awqat-time-functions times)
				(setq times (append times (list (awqat-pretty-time (apply f (list date)))))))))))

;;; Time Calculation Functions
(defun awqat-fajr (date)
  "Calculates the time of fajr for DATE."
  (car (awqat-sunrise-sunset-angle date awqat-fajr-angle)))

(defun awqat-imsak (date)
  "Calculates the time of imsak (sunrise) for DATE."
  (car (awqat-sunrise-sunset date)))

(defun awqat-dhuhr (date)
  "Calculate the time of Zuhr on DATE."
  (awqat-solar-noon date))				;

;; TODO : Pretty 

(defun awqat-asr (date &optional hanafi)
  "Return the time for asr on DATE (if HANAFI 2x len + noon shadow)."
  (let* ((s (awqat-length-of-shadow-at-noon date))
		 (l (+ (if hanafi 2 1) s))
		 (h (awqat-rad-to-deg (atan (/ 1 l)))))
	(cadr (awqat-sunrise-sunset-angle date h))))

(defun awqat-maghrib (date)
  "Return the time for maghrib (sunset) on DATE."
  (cadr (awqat-sunrise-sunset date)))

(defun awqat-isha (date)
  "Calculates the time of isha for DATE."
  (cadr (awqat-sunrise-sunset-angle date awqat-isha-angle)))


;;; Astronomical calculations
(defun awqat-height-of-sun-at-noon (date)
  "Calculates the height of at solar noon on DATE."
  (let* ((exact-local-noon (solar-exact-local-noon date))
		 (t0 (solar-julian-ut-centuries (car exact-local-noon)))
		 (ut (cadr exact-local-noon))

		 (hnoon (solar-horizontal-coordinates (list t0 ut)
											  (calendar-latitude)
											  (calendar-longitude) t)))
	hnoon))

(defun awqat-solar-noon (date)
  "Calculate the time of Zuhr on DATE."
  (let* ((exact-local-noon (solar-exact-local-noon date))
		 (midday-time (dst-adjust-time date (cadr exact-local-noon)))
		 (base-time (cadr midday-time)))
    (setcar (cdr midday-time) (+ base-time (/ calendar-time-zone 60.0)))
	(cdr midday-time)))

(defun awqat-sunrise-sunset (date)
  "Calculate the times of maghrib and imsak for DATE."
  (awqat-sunrise-sunset-angle date awqat-sunrise-sunset-below-angle))

(defun awqat-length-of-shadow-at-noon (date)
  "Calculates the relative length of an objects shadow at solar noon on DATE."
  (let ((hn (cadr (awqat-height-of-sun-at-noon date))))
	(/ 1 (tan (awqat-deg-to-rad hn)))))

(defun awqat-sunrise-sunset-angle (date angle)
  "Calculate the sunrise and sunset on given DATE (ex (7 22 2019)) with ANGLE above horizon."
  (let* ((exact-local-noon (solar-exact-local-noon date))
         ;; Get the time from the 2000 epoch.
         (t0 (solar-julian-ut-centuries (car exact-local-noon)))
         ;; Store the sidereal time at Greenwich at midnight of UT time.
         ;; Find if summer or winter slightly above the equator.
         (equator-rise-set
          (progn (setq solar-sidereal-time-greenwich-midnight
                       (solar-sidereal-time t0))
                 (solar-sunrise-and-sunset
                  (list t0 (cadr exact-local-noon))
                  1.0
                  (calendar-longitude) 0)))
         ;; Store the spring/summer information, compute sunrise and
         ;; sunset (two first components of rise-set).  Length of day
         ;; is the third component (it is only the difference between
         ;; sunset and sunrise when there is a sunset and a sunrise)
         (rise-set
          (progn
            (setq solar-northern-spring-or-summer-season
                  (> (nth 2 equator-rise-set) 12))
			(solar-sunrise-and-sunset
             (list t0 (cadr exact-local-noon))
             (calendar-latitude)
             (calendar-longitude) angle))) ;; This is the parameter that.
         (rise-time (car rise-set))
         (adj-rise (if rise-time (dst-adjust-time date rise-time)))
         (set-time (cadr rise-set))
         (adj-set (if set-time (dst-adjust-time date set-time)))
         (length (nth 2 rise-set)))
	(print exact-local-noon)
	(print t0)
	(print equator-rise-set)
	(print rise-set)
    (list
     (and rise-time (cdr adj-rise))
     (and set-time (cdr adj-set))
     (solar-daylight length))))

;;; Mathematical Functions
(defun awqat-rad-to-deg (x)
  "Convert X from radians to degrees."
  (/ (* x 180) float-pi))

(defun awqat-deg-to-rad (x)
  "Convert X from radians to degrees."
  (/ (* x float-pi) 180))

(defun awqat-pretty-time (time)
  "Format TIME to human readable form."
  (apply 'solar-time-string time))

(provide 'awqat)
;;; awqat.el ends here
