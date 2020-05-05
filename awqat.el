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

(defvar awqat-fajr-angle -18.15
  "The angle below sunrise used to calculate the time of fajr.")

(defvar awqat-isha-angle -13.94
  "The angle below sunset used to calculate the time of isha.")

(defvar awqat-fajr-before-offset 1.81
  "The time in hours before sunrise the Fajr prayer comes in.

This is applicable only when fajr offset is selected.")

(defvar awqat-isha-after-sunset 1.333
  "The time in hours after sunset the Isha prayer comes in.

This is applicable only when isaha offset is selected.")

(defvar awqat-sunrise-sunset-below-angle -1.66
  "The angle used for determining the sunrise and sunset.
This is not zero as when angle is 0, sun is still visible.")

(defvar awqat-asr-hanafi t
  "The offset applied for the 6 times.")

(defvar awqat-prayer-safety-offsets (make-list 6 0.0)
  "The offset in minutes applied for the 6 times.")

(defvar awqat--prayer-funs
  (list #'awqat--prayer-fajr
		#'awqat--prayer-sunrise
		#'awqat--prayer-dhuhr
		#'awqat--prayer-asr
		#'awqat--prayer-maghrib
		#'awqat--prayer-isha)
  "The offset applied for the 6 times.")


;;; Pre-configuration functions

(defun awqat-use-angle-based-method ()
  "Set the calculation for isha and fajr to be angle based."
  (setq awqat--prayer-funs (list #'awqat--prayer-fajr
								 #'awqat--prayer-sunrise
								 #'awqat--prayer-dhuhr
								 #'awqat--prayer-asr
								 #'awqat--prayer-maghrib
								 #'awqat--prayer-isha)))

(defun awqat-use-time-offset-method ()
  "Set the calculation for isha and fajr to be hours before/after based."
  (setq awqat--prayer-funs (list #'awqat--prayer-fajr-offset
								 #'awqat--prayer-sunrise
								 #'awqat--prayer-dhuhr
								 #'awqat--prayer-asr
								 #'awqat--prayer-maghrib
								 #'awqat--prayer-isha-offset)))

(defun awqat-set-preset-diyanet ()
  "Set the calculation method to be simmilar to the Muslim Pro app."
  (setq awqat--prayer-funs (list #'awqat--prayer-fajr
								 #'awqat--prayer-sunrise
								 #'awqat--prayer-dhuhr
								 #'awqat--prayer-asr
								 #'awqat--prayer-maghrib
								 #'awqat--prayer-isha))
  (setq awqat-fajr-angle -18.15)
  (setq awqat-isha-angle -13.94))

(defun awqat-set-preset-muslim-pro ()
  "Set the calculation method to be simmilar to the Muslim Pro app."
  (awqat--preset-with-angles -18.13 -16.3))

(defun awqat-set-preset-muslim-world-league ()
  "Set the calculation method to be simmilar to the muslim world league."
  (awqat--preset-with-angles -18.0 -17))

(defun awqat-set-preset-karachi-university-of-islamic-sciences ()
  "Set the calculation method to be simmilar to the karachi university of islamic sciences."
  (awqat--preset-with-angles -18.0 -18.0))

(defun awqat-set-preset-umm-al-qura ()
  "Set the calculation method to be simmilar to umm al qura."
  (awqat--preset-with-angles -18.5 -19.0))

(defun awqat-set-preset-jakim ()
  "Set the calculation method to be simmilar to JAKIM."
  (awqat--preset-with-angles -20.0 -18.0))

(defun awqat-set-preset-spiritual-administration-of-musilms-russia ()
  "Set the calculation method to be simmilar to the SAMR."
  (awqat--preset-with-angles -16.0 -15.0))

(defun awqat-set-preset-union-des-organisations-islamiques-de-france ()
  "Set the calculation method to be simmilar to the Union des Organisations Islamiques de France."
  (awqat--preset-with-angles -16.0 -15.0))

(defun awqat--preset-with-angles (fajr isha)
  "Use the standard angle method calculation with FAJR and ISHA angles."
  (setq awqat--prayer-funs (list #'awqat--prayer-fajr
								 #'awqat--prayer-sunrise
								 #'awqat--prayer-dhuhr
								 #'awqat--prayer-asr
								 #'awqat--prayer-maghrib
								 #'awqat--prayer-isha))
  (setq awqat-fajr-angle fajr)
  (setq awqat-isha-angle isha))


;;; UI/Interactive functions and helpers.

(defun awqat-times-for-day ()
  "Calculate adn display all of the set times for the current day."
  (interactive)
  (let* ((today (awqat--today))
		 (h-m (mapcar #'string-to-number (split-string (format-time-string "%H:%M") ":")))
		 (now (+ (car h-m) (/ (cadr h-m) 60.0)))
		 (ret-str "")
		 (time-remaining)
		 (earliest-time))
	(dolist (time-idx (number-sequence 0 (1- (length awqat--prayer-funs))) ret-str)
	  (let ((time (awqat--prayer-time today time-idx)))
		(setq ret-str (concat ret-str (awqat-pretty-time time) "  "))
		(when (and (not time-remaining) (< now (car time)))
		  (setq time-remaining (- (car time) now)))
		(when (not earliest-time)
		  (setq earliest-time time))))
	(when (not time-remaining)
	  (setq time-remaining (mod (- (+ 24.0 (car earliest-time)) now) 24)))
	(message "%s   Time remaining> %s"
			 ret-str
			 (propertize (format "%d:%02d"
								 (floor time-remaining)
								 (* 60 (- time-remaining (floor time-remaining))))
						 'face (if (< time-remaining 0.5) '(:foreground "red") '())))))

(defun awqat--today ()
  "Return the current date (used throught the program) in the require format (M D Y)."
  (list
   (string-to-number (format-time-string "%m"))
   (string-to-number (format-time-string "%d"))
   (string-to-number (format-time-string "%Y"))))

;;; Prayer Calculations ------------------------------------------------------------------

(defun awqat--prayer-time (date prayer)
  "Calculate a time for PRAYER idx on given DATE, applying safety offsets."
  (let* ((offset (nth prayer awqat-prayer-safety-offsets))
		 (fun (nth prayer awqat--prayer-funs))
		 (time (apply fun (list date))))
	(list (+ (car time) (/ offset 60.0)) (cadr time))))

;; The following functions can be put into the awqat--prayer-funs list.
;; Fajr

(defun awqat--prayer-fajr (date)
  "Calculate the time of fajr for DATE using standard angle method."
  (car (awqat-sunrise-sunset-angle date awqat-fajr-angle)))

(defun awqat--prayer-fajr-diyanet (date)
  "Calculate the time of fajr for DATE using third-portion if lat > 45."
  (if (< calendar-latitude 45.0)
	  (awqat--prayer-fajr date)
	(let ((third-portion (awqat--third-portion-calc date awqat-fajr-angle))
		  (sunrise (awqat--sunrise date))
		  (timezone (awqat--timezone date)))
	  (list (if (> third-portion 1.3333)
				(- sunrise 1.3333)
			  (+ sunrise third-portion))
			timezone))))

(defun awqat--prayer-fajr-angle (date)
  "Calculate the time of fajr for DATE using angle method if necessary."
  (if (awqat--use-angle-method-p date)
	  (let ((offset (awqat--angle-approx-offset date awqat-fajr-angle))
			(timezone (awqat--timezone date))
			(sunrise (awqat--sunrise date)))
		(list (- sunrise offset) timezone))
	(awqat--prayer-fajr date)))

(defun awqat--prayer-fajr-offset (date)
  "Calculate the time of fajr based on fixed time for given DATE."
  (let ((sunrise (awqat--sunset date))
		(timezone (awqat--sunset date)))
	(list (- sunrise awqat-fajr-before-offset)
		  timezone)))

;; Sunrise

(defun awqat--prayer-sunrise (date)
  "Calculate the time of the sunrise on a given DATE."
  (list (awqat--sunrise date)
		(awqat--timezone date)))

;; Dhuhr

(defun awqat--prayer-dhuhr (date)
  "Calculate the prayer time for dhuhr on a given DATE."
  (awqat-solar-noon date))

;; Asr

(defun awqat--prayer-asr (date)
  "Calculate the time of asr on DATE.

If `awqat-asr-hanafi' is non-nil, use double the length of noon shadow."
  (let* ((s (awqat-length-of-shadow-at-noon date))
		 (l (+ (if awqat-asr-hanafi 2 1) s))
		 (h (awqat-rad-to-deg (atan (/ 1 l)))))
	(cadr (awqat-sunrise-sunset-angle date h))))

;; Maghrib

(defun awqat--prayer-maghrib (date)
  "Calculate the time of maghrib on DATE."
  (list (awqat--sunset date)
		(awqat--timezone date)))

;; Isha

(defun awqat--prayer-isha (date)
  "Calculate the time of isha on DATE using the standard angle method."
  (cadr (awqat-sunrise-sunset-angle date awqat-isha-angle)))

(defun awqat--prayer-isha-diyanet (date)
  "Calculate the time of fajr for DATE using third-portion if lat > 45."
  (if (< calendar-latitude 45.0)
	  (awqat--prayer-isha date)
	(let ((third-portion (awqat--third-portion-calc date awqat-isha-angle))
		  (sunset (awqat--sunset date))
		  (timezone (awqat--timezone date)))
	  (list (if (> third-portion 1.3333)
				(+ sunset 1.3333)
			  (+ third-portion sunset))
			timezone))))

(defun awqat--prayer-isha-angle (date)
  "Calculate the time of fajr for DATE using angle method if necessary."
  (if (awqat--use-angle-method-p date)
	  (let ((offset (awqat--angle-approx-offset date awqat-isha-angle))
			(timezone (awqat--timezone date))
			(sunset (awqat--sunrise date)))
		(list (+ sunset offset) timezone))
	(awqat--prayer-isha date)))

(defun awqat--prayer-isha-offset (date)
  "Calculate the time of fajr based on fixed time for given DATE."
  (let ((sunset (awqat--sunset date))
		(timezone (awqat--sunset date)))
	(list (+ sunset awqat-isha-after-sunset)
		  timezone)))

;;; Time Calculations --------------------------------------------------------------------

(defun awqat--angle-approx-offset (date angle)
  "Calculate the time of isha for DATE at with ANGLE using andle method.

This method calculates the time based on a portion of angle/60 of the night."
  (let* ((night-duration (awqat-duration-of-night date)))
	(/ (* (abs angle) night-duration) 60)))

(defun awqat--third-portion-calc (date angle)
  "Calculate the time of isha for DATE with ANGLE using third portion method.
Calculate fajr if IS-ISHA is false.

This method is considered according only if calendar latitude is greater than 45deg."
  (let* ((sunset (awqat--sunset date))
		 (fecri-sadik (caadr (awqat-sunrise-sunset-angle date angle))))
	(/ (mod (- (+ fecri-sadik 24.0) sunset) 24.0) 3)))

(defun awqat--timezone (date)
  "Return the timezone used to calculate times on DATE."
  (cadar (awqat-sunrise-sunset date)))

(defun awqat--sunrise (date)
  "Calculate the sunrise for a given DATE."
  (caar (awqat-sunrise-sunset date)))

(defun awqat--sunset (date)
  "Calculate the sunset for a given DATE."
  (caadr (awqat-sunrise-sunset date)))

(defun awqat--use-angle-method-p (date)
  "Determine if on DATE, the angle method for isha/fajr should be used."
  (let* ((isha (caadr (awqat-sunrise-sunset-angle date awqat-isha-angle)))
		 (fajr (caar (awqat-sunrise-sunset-angle date awqat-fajr-angle))))
	(if (or (not isha) (not fajr))
		t
	  (let* ((isha-offset (awqat--angle-approx-offset date awqat-isha-angle))
			 (sunset (awqat--sunset date))
			 (isha-angle (+ sunset isha-offset))
			 (fajr-offset (awqat--angle-approx-offset date awqat-fajr-angle))
			 (sunrise (awqat--sunrise date))
			 (fajr-angle (- sunrise fajr-offset))
			 (isha-fajr-diff (mod (- (+ 24.0 fajr) isha) 24)) ;
			 (isha-fajr-angle-diff (- (+ 24.0 fajr-angle) isha-angle))) ;
		(> isha-fajr-angle-diff isha-fajr-diff)))))

;;; Astronomical Calculations -----------------------------------------------------------

(defun awqat-sunrise-sunset (date)
  "Calculate the times of maghrib and imsak for DATE."
  (awqat-sunrise-sunset-angle date awqat-sunrise-sunset-below-angle))

(defun awqat-height-of-sun-at-noon (date)
  "Calculates the height of at solar noon on DATE."
  (let* ((exact-local-noon (solar-exact-local-noon date))
		 (t0 (solar-julian-ut-centuries (car exact-local-noon)))
		 (ut (cadr exact-local-noon))

		 (hnoon (solar-horizontal-coordinates (list t0 ut)
											  (calendar-latitude)
											  (calendar-longitude) t)))
	hnoon))

;; modified from solar.el
(defun awqat-sunrise-sunset-angle (date angle)
  "Calculate the sunrise and sunset on given DATE (ex (7 22 2019)) with ANGLE above horizon."
  (let ((res (let* ((exact-local-noon (solar-exact-local-noon date))
				 (t0 (solar-julian-ut-centuries (car exact-local-noon)))
				 (equator-rise-set
				  (progn (setq solar-sidereal-time-greenwich-midnight
							   (solar-sidereal-time t0))
						 (solar-sunrise-and-sunset
						  (list t0 (cadr exact-local-noon))
						  1.0
						  (calendar-longitude) 0)))
				 (rise-set
				  (progn
					(setq solar-northern-spring-or-summer-season
						  (> (nth 2 equator-rise-set) 12))
					(solar-sunrise-and-sunset
					 (list t0 (cadr exact-local-noon))
					 (calendar-latitude)
					 (calendar-longitude) angle)))
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
  "Return the duration from sunset to sunrise for a given DATE."
  (let* ((sunrise-sunset (awqat-sunrise-sunset date))
		 (sunrise (caar sunrise-sunset))
		 (sunset (caadr sunrise-sunset)))
	(- (+ sunrise 24.0) sunset)))

(defun awqat-solar-noon (date)
  "Calculate the time of Zuhr on DATE."
  (let* ((exact-local-noon (solar-exact-local-noon date))
		 (midday-time (dst-adjust-time date (cadr exact-local-noon)))
		 (base-time (cadr midday-time)))
    (setcar (cdr midday-time) (+ base-time (/ calendar-time-zone 60.0)))
	(cdr midday-time)))

(defun awqat-length-of-shadow-at-noon (date)
  "Calculates the relative length of an objects shadow at solar noon on DATE."
  (let ((hn (cadr (awqat-height-of-sun-at-noon date))))
	(/ 1 (tan (awqat-deg-to-rad hn)))))

(defun awqat-rad-to-deg (x)
  "Convert X from radians to degrees."
  (/ (* x 180) float-pi))

(defun awqat-deg-to-rad (x)
  "Convert X from radians to degrees."
  (/ (* x float-pi) 180))

(defun awqat-pretty-time (time &optional flag)
  "Format TIME to human readable form and colored if FLAG is not nil.

If FLAG is 'skip then return empty string."
  (if (eq flag 'skip)
	  ""
	(let ((pretty-time (if time (apply 'solar-time-string time) "---")))
	  (if flag
		  (propertize pretty-time 'face '(:background "yellow"))
		pretty-time))))

(provide 'awqat)
;;; awqat2.el ends here
