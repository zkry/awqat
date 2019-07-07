;;; awqat.el --- Calculations of positions of the sun -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Zachary Romero

;; Author: Zachary Romero <zacromero@posteo.net>
;; Url: http://github.com/zkry/

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

(defun atan2 (x y)
  "Return the atan2 in radians of X and Y."
  (cond ((> x 0) (atan (/ y x)))
		((and (< x 0) (>= y 0)) (+ (atan (/ y x)) float-pi) )
		((and (< x 0) (< y 0)) (- (atan (/ y x)) float-pi))
		((and (= x 0) (> y 0)) (/ float-pi 2))
		((and (= x 0) (< y 0)) (- (/ float-pi 2)))
		((and (= x 0) (= y 0)) (error "Func atan2(0,0) is undefined"))))

(defun atan2-deg (x y)
  "Return the atan2 in degrees of X and Y."
  (radian-to-degree (atan2 x y)))

(defun radian-to-degree (r)
  "Convert radians R to degrees."
  (/ (* r 180) float-pi))

;; Fajr
;; Imsaq
;; Duhur
;; Asr
;; Maghrib
;; Isha
(setq calendar-latitude 52.4939)
(setq calendar-longitude 13.4364)

(defun salah-sunrise-sunset (date)
  "Calculate the sunrise and sunset on given DATE (ex (7 22 2019))with custom ??? parameter."
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
             (calendar-longitude) -1.66)))
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
     (and rise-time (calendar-date-equal date (car adj-rise)) (cdr adj-rise))
     (and set-time (calendar-date-equal date (car adj-set)) (cdr adj-set))
     (solar-daylight length))))

(defun acot (x)
  (- (/ float-pi 2.0) (atan x)))

(defun degrees-to-radian (x)
  (/ (* x float-pi) 180.0))

(defun asr (date x)
  (let* ((exact-local-noon (solar-exact-local-noon date))
		 (t0 (solar-julian-ut-centuries (car exact-local-noon)))
		 (eq (solar-ecliptic-coordinates t0 nil))
		 (L (degrees-to-radian (car eq)))
         (D (degrees-to-radian (solar-declination (car eq) (cadr eq)))))
    (print eq)
	(print L)
	(print D)
    (/ (acos (/ (- (sin (acot (+ x (tan (- L D))))) (* (sin L) (sin D)))
				(* (cos L) (cos D))))
	   15.0)))



(defun salah-fajr (date)
  "Calculate the time of the Fajr prayer on DATE.")

(defun salah-sunrise (date)
  "Calculate the sunrise on DATE.")

(defun salah-zuhr (date)
  "Calculate the time of Zuhr on DATE."
  (let* ((exact-local-noon (solar-exact-local-noon date))
		 (midday-time (dst-adjust-time date (cadr exact-local-noon)))
		 (base-time (cadr midday-time)))
    (setcar (cdr midday-time) (+ base-time (/ calendar-time-zone 60.0)))
    (apply 'solar-time-string (cdr midday-time))))

(defun salah-asr (date)
  "Calculate the time of the Asr prayer on DATE.")

(defun salah-maghrib (date)
  "Calculate the time of the Maghrib prayer on DATE.")

(defun salah-isha (date)
  "Calculate the time of the Isha prayer on DATE.")

;;; salah.el ends here
