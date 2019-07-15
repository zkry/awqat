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


;; NOTE: DATE = '(M D Y)

(defvar awqat-time-functions '()
  "Stores a list of the time functions to use.")
(setq awqat-time-functions (list 'awqat-fajr 'awqat-imsak))
;;; General settings
(defvar awqat-sunrise-sunset-below-angle -1.66)


;;; Islamic prayer times settings
(defvar awqat-fajr-angle -18.0)
(defvar awqat-isha-angle -17.0)
(defvar awqat-use-angle-calculation t)
(defvar awqat-prayer-safety-offsets (make-list 6 0))

;;; Application commands
(defun awqat-times-for-day ()
  "Calculate all of the times for current day."
  (interactive)
  (let* ((date (list
			   (string-to-number (format-time-string "%m"))
			   (string-to-number (format-time-string "%d"))
			   (string-to-number (format-time-string "%Y"))))
		(hour-minute (mapcar #'string-to-number (split-string (format-time-string "%H:%M") ":")))
		(current-time (+ (car hour-minute) (/ (cadr hour-minute) 60.0)))
		(times "")
		(time-remaining))
	(dolist (f awqat-time-functions times)
	  (let ((time (apply f (list date))))
		(setq times (concat times (awqat-pretty-time time
													 (and (not time-remaining)
														  (< current-time (car time))
														  (setq time-remaining
																(- (car time) current-time))))
							"  "))))
	(message "%s   Time remaining> %s"
			 times
			 (propertize (format "%d:%02d"
								 (floor time-remaining)
								 (* 60 (- time-remaining (floor time-remaining))))
						 'face (if (< time-remaining 0.5) '(:foreground "red") '())))))

;;; Application functions
(defun awqat-use-preset (preset)
  (cond ((eq 'muslim-world-leauge preset)
		 (setq awqat-fajr-angle -18.0)
		 (setq awqat-isha-angle -17.0)
		 (setq awqat-use-angle-calculation t)
		 (setq awqat-sunrise-sunset-below-angle -1.02)
		 (setq awqat-prayer-safety-offsets '(0 0 0 0 0 0))
		 (setq awqat-time-functions (list #'awqat-fajr #'awqat-imsak
										  #'awqat-dhuhr #'awqat-asr
										  #'awqat-maghrib #'awqat-isha)))
		((eq 'diyanet preset)
		 (setq awqat-fajr-angle -18.0)
		 (setq awqat-isha-angle -17.05)
		 (setq awqat-use-angle-calculation nil)
		 (setq awqat-sunrise-sunset-below-angle -1.5)
		 (setq awqat-prayer-safety-offsets '(0 -1.0 5.0 4.0 2.0 0.0))
		 (setq awqat-time-functions (list #'awqat-fajr--diyanet #'awqat-imsak
										  #'awqat-dhuhr #'awqat-asr
										  #'awqat-maghrib #'awqat-isha--diyanet)))
		((eq 'isna preset)
		 (print "isna"))
		((eq 'egypt preset)
		 (print "egypt"))
		((eq 'um-alqwa-taqwee preset)
		 (print "umaltaqweem"))

		((eq 'igmg preset)
		 (print "IGMG"))
		((eq 'uois-karachi preset)
		 (print "University of Islamic Sciences, Karachi"))
		((eq 'udoidf preset)
		 (print "Union des Organisations Islamiques de France"))
		((eq 'kuwait preset)
		 (print "Kuwait"))
		((eq 'gulf preset)
		 (print "Gulf"))
		((eq 'qatar preset)
		 (print "Qatar"))
		(t (print "none"))))

(awqat-use-preset 'muslim-world-leauge)

;;; Time Calculation Functions
(defun awqat-fajr (date)
  "Calculates the time of fajr for DATE."
  (awqat--apply-safety-time 'fajr (if (awqat--use-angle-method-p date)
									  (awqat-fajr--angle date)
									(car (awqat-sunrise-sunset-angle date awqat-fajr-angle)))))

(defun awqat-fajr--diyanet (date)
  "Calculate the isha time on DATE according to diyanet algorithm."
  (if (< calendar-latitude 45.0)
	  ;; Run normal isha if lat under 45deg.
	  (awqat-fajr date)
	(let* ((sunrise-sunset (awqat-sunrise-sunset date))
		   (sunrise (caar sunrise-sunset))
		   (sunset (caadr sunrise-sunset))
		   (fecri-sadik (caadr (awqat-sunrise-sunset-angle date awqat-fajr-angle)))
		   (third-portion (+ (/ (mod (- (+ fecri-sadik 24.0) sunset) 24.0) 3))))
	  (if (> third-portion 1.33333)
		  (list (- sunrise 1.33333) (cadar sunrise-sunset))
		(list (- sunrise third-portion 0.1666666) (cadar sunrise-sunset))))))

(defun awqat-fajr--angle (date)
  "Calculates the time of isha for DATE."
  (let* ((sunrise-sunset (awqat-sunrise-sunset date))
		 (sunrise (caar sunrise-sunset))
		 (night-duration (awqat-duration-of-night date))
		 (fajr-offset (/ (* (abs awqat-fajr-angle) night-duration) 60)))
	(list (- sunrise fajr-offset) (cadar sunrise-sunset))))

(defun awqat-imsak (date)
  "Calculates the time of imsak (sunrise) for DATE."
  (awqat--apply-safety-time 'imsak (car (awqat-sunrise-sunset date))))

(defun awqat-dhuhr (date)
  "Calculate the time of Zuhr on DATE."
  (awqat--apply-safety-time 'dhuhr (awqat-solar-noon date)))				;

(defun awqat-asr (date &optional hanafi)
  "Return the time for asr on DATE (if HANAFI 2x len + noon shadow)."
  (awqat--apply-safety-time 'asr (let* ((s (awqat-length-of-shadow-at-noon date))
										(l (+ (if hanafi 2 1) s))
										(h (awqat-rad-to-deg (atan (/ 1 l)))))
								   (cadr (awqat-sunrise-sunset-angle date h)))))

(defun awqat-maghrib (date)
  "Return the time for maghrib (sunset) on DATE."
  (awqat--apply-safety-time 'maghrib (cadr (awqat-sunrise-sunset date))))

(defun awqat-isha (date)
  "Calculates the time of isha for DATE."
  (awqat--apply-safety-time 'isha (if (awqat--use-angle-method-p date)
									  (awqat-isha--angle date)
									(cadr (awqat-sunrise-sunset-angle date awqat-isha-angle)))))

(defun awqat-isha--diyanet (date)
  "Calculate the isha time on DATE according to diyanet algorithm."
  (awqat--apply-safety-time
   'isha (if (< calendar-latitude 45.0)
			 ;; Run normal isha if lat under 45deg.
			 (awqat-isha date)
		   (let* ((sunrise-sunset (awqat-sunrise-sunset date))
				  (sunset (caadr sunrise-sunset))
				  (fecri-sadik (caadr (awqat-sunrise-sunset-angle date awqat-isha-angle)))
				  (third-portion (/ (mod (- (+ fecri-sadik 24.0) sunset) 24.0) 3)))
			 (if (> third-portion 1.33333)
				 (list (+ sunset 1.33333) (cadar sunrise-sunset))
			   (list (+ third-portion sunset) (cadar sunrise-sunset)))))))

(defun awqat-isha--angle (date)
  "Calculates the time of isha for DATE using angle approx."
  (if awqat-use-angle-calculation
	  (let* ((sunrise-sunset (awqat-sunrise-sunset date))
			 (sunset (caadr sunrise-sunset))
			 (night-duration (awqat-duration-of-night date))
			 (isha-offset (/ (* (abs awqat-isha-angle) night-duration) 60)))
		(list (+ sunset isha-offset) (cadar sunrise-sunset)))
	nil))



(defun awqat--use-angle-method-p (date)
  "Determine if on DATE, the angle method for isha/fajr should be used."
  ;; TODO: Add case: (lat < X) -> return nil
  (if awqat-use-angle-calculation
	  (let* ((isha (caadr (awqat-sunrise-sunset-angle date awqat-isha-angle)))
			 (fajr (caar (awqat-sunrise-sunset-angle date awqat-fajr-angle))))
		(if (or (not isha) (not fajr))
			t
		  (let* ((isha-angle (car (awqat-isha--angle date)))
				 (fajr-angle (car (awqat-fajr--angle date)))
				 (isha-fajr-diff (mod (- (+ 24.0 fajr) isha) 24))
				 (isha-fajr-angle-diff (- (+ 24.0 fajr-angle) isha-angle)))
			(> isha-fajr-angle-diff isha-fajr-diff))))
	nil))

(defun awqat--apply-safety-time (prayer time)
  "Apply safty offset to TIME according to PRAYER.

TIME is a list with first element in hours, and second a string
time zone.  Prayer is a symbol for prayer time."
  (setcar time (+ (/ (awqat--get-safety-time prayer) 60) (car time)))
  time)

(defun awqat--get-safety-time (prayer)
  "Return safety time for given PRAYER."
  (cond ((eq prayer 'fajr) (nth 0 awqat-prayer-safety-offsets))
		((eq prayer 'imsak) (nth 1 awqat-prayer-safety-offsets))
		((eq prayer 'dhuhr) (nth 2 awqat-prayer-safety-offsets))
		((eq prayer 'asr) (nth 3 awqat-prayer-safety-offsets))
		((eq prayer 'maghrib) (nth 4 awqat-prayer-safety-offsets))
		((eq prayer 'isha) (nth 5 awqat-prayer-safety-offsets))
		(t (error "Invalid prayer name"))))

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

(defun awqat-lowest-solar-angle (date)
  "Calculate the lowest angle that the sun reaches in a day."
  (let* ((exact-local-noon (solar-exact-local-noon date))
		 (t0 (solar-julian-ut-centuries (car exact-local-noon)))
		 (ut (+ 12.0 (cadr exact-local-noon)))

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
  (let ((res (let* ((exact-local-noon (solar-exact-local-noon date))
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
			(list
			 (and rise-time (cdr adj-rise))
			 (and set-time (cdr adj-set))
			 (solar-daylight length)))))
	(if (car res)
		res
	  (let ((solar-noon (awqat-solar-noon date)))
		(list solar-noon solar-noon)))))

(defun awqat-duration-of-night (date)
  "Returns the duration from sunset to sunrise for a given date."
  (let* ((sunrise-sunset (awqat-sunrise-sunset date))
		 (sunrise (caar sunrise-sunset))
		 (sunset (caadr sunrise-sunset)))
	(- (+ sunrise 24.0) sunset)))

;;; Mathematical Functions
(defun awqat-rad-to-deg (x)
  "Convert X from radians to degrees."
  (/ (* x 180) float-pi))

(defun awqat-deg-to-rad (x)
  "Convert X from radians to degrees."
  (/ (* x float-pi) 180))

(defun awqat-pretty-time (time &optional color)
  "Format TIME to human readable form and colored if COLOR is not nil."
  (let ((pretty-time (if time (apply 'solar-time-string time) "---")))
	(if color
		(propertize pretty-time 'face '(:background "yellow"))
	  pretty-time)))

(provide 'awqat)
;;; awqat.el ends here
