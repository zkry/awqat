;;; awqat.el --- Islamic prayer times -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022 Zachary Romero

;; Package-Requires: ((emacs "27.1") (s "1.13.0"))
;; Package-Version: 20250131.162501
;; Author: Zachary Romero <zacromero@posteo.net>
;; Contributor: Abdelhak Bougouffa <abdelhak.bougouffa@universite-paris-saclay.fr>
;; Version: 0.2.0
;; Homepage: http://github.com/zkry/awqat

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

;; This package provides the `awqat-times-for-day' command which
;; displays the Islamic prayer times for the current day based on the
;; configured latitude and longitude.  You should set the values of
;; the `calendar-latitude' and `calendar-longitude' for the
;; calculations to work properly.  Refer to the documentation for how
;; to tweak the calculations of the prayer times.

;;; Code:

(require 'simple)
(require 'solar)
(require 'calendar)
(require 'cal-islam)
(require 's)
(require 'alert)


(defgroup awqat nil
  "A package to calculate the five daily Islamic prayer times in Emacs."
  :prefix "awqat-"
  :group 'awqat)

(defvar awqat-todays-prayer-times nil
  "Holds today's prayer times.")

(defcustom awqat-fajr-angle -18.0
  "The Fajr zenith angle offset (in degrees) below horizon.

Applicable when calculating Fajr in angle-based approaches.

The value of this parameter changes from an approach to another,
it is determined by astronomical observation and
religious jurisprudence (al-Fiqh).

It can also differ from a geographical region to anther.

Prefer using using predefined presets suitable for your geographic area."
  :type 'float
  :group 'awqat)

(defcustom awqat-isha-angle -17.0
  "The Isha zenith angle offset (in degrees) below horizon.

Applicable when calculating Isha in angle-based approaches.

The value of this parameter changes from an approach to another,
it is determined by astronomical observation,
and religious jurisprudence (al-Fiqh).

It can also differ from a geographical region to anther.

Prefer using using predefined presets suitable for your geographic area."
  :type 'float
  :group 'awqat)

(defcustom awqat-fajr-before-sunrise-offset 1.81
  "The Fajr time offset (in hours) before sunrise.

This is applicable only when calculating Fajr offset-based approaches."
  :type 'float
  :group 'awqat)

(defcustom awqat-isha-after-sunset-offset 1.333
  "The Fajr time offset (in hours) after sunset.

This is applicable only when calculating Isha offset-based approaches."
  :type 'float
  :group 'awqat)

(defcustom awqat-isha-moonsighting-method 'shafaq
  "Method to use for Isha in Moonsighting Committee Wourldwide method.
Can be `'shafaq-ahmar', `'shafaq-abyad', or `'shafaq' (which is a combination
of Shafaq Ahmar and Abyad for high latitudes).

For detailed information, see \"Syed Khalid Shaukat, Fajr and Isha, Sep 2015\"."
  :type '(choice (const shafaq) (const shafaq-abyad) (const shafaq-ahmar))
  :group 'awqat)

(defcustom awqat-sunrise-sunset-angle -0.833
  "The sunrise/sunset zenith angle offset below horizon.

Used to determine the sunrise and sunset (Maghreb).
A zero value corresponds to the sun being at zenith=90°,
which means that the sun circle is still visible.
The apparent radius of the sun at the horizon is 16 arcminutes,
and the average refraction is known to be 34 arcminutes,
which gives an offset of 50 arcminutes, hence the 0.833° value."
  :type 'float
  :group 'awqat)

(defcustom awqat-asr-hanafi nil
  "Use the Hanafi jurisprudence (al-Fiqh al-Hanafi) for Asr time.

Default value is nil, corresponding to the majority opinion (al-Jomhor),
including the Maliki, Shafii, and Hambali schools of thought."
  :type 'boolean
  :group 'awqat)

(defcustom awqat-prayer-safety-offsets (make-list 6 0.0)
  "The offset in minutes applied for the six times.

List ordered as: (Fajr Sunrise Dhuhr Asr Maghrib Isha)."
  :type 'list
  :group 'awqat)

(defcustom awqat-prayer-funs
  (list #'awqat--prayer-fajr
        #'awqat--prayer-sunrise
        #'awqat--prayer-dhuhr
        #'awqat--prayer-asr
        #'awqat--prayer-maghrib
        #'awqat--prayer-isha)
  "The functions used to calculate each time, a list of six elements."
  :type 'list
  :group 'awqat)

(make-obsolete-variable 'awqat--prayer-funs 'awqat-prayer-funs "1.0.0")

(defvar awqat--prayer-names
  (list "Fajr"
        "Sunrise"
        "Dhuhr"
        "Asr"
        "Maghrib"
        "Isha")
  "Names of the six times.")

;;; Notification settings
(defcustom awqat-notifications-enabled t
  "Whether to send desktop notifications for prayer times."
  :type 'boolean
  :group 'awqat)

(defcustom awqat-notifications-for-times '(t t t t t t)
  "List of booleans indicating whether to send notifications for each prayer time.
The list order corresponds to: (Fajr Sunrise Dhuhr Asr Maghrib Isha)."
  :type '(repeat boolean)
  :group 'awqat)

(defcustom awqat-notification-title "Prayer Time"
  "Title for the prayer time notification."
  :type 'string
  :group 'awqat)

(defcustom awqat-notification-message-format "It's time for %s"
  "Format string for prayer time notification message.
%s will be replaced with the prayer name."
  :type 'string
  :group 'awqat)

(defcustom awqat-pre-notifications-enabled nil
  "Whether to send notifications before prayer times."
  :type 'boolean
  :group 'awqat)

(defcustom awqat-pre-notification-minutes 15
  "Minutes before prayer time to send the pre-notification."
  :type 'integer
  :group 'awqat)

(defcustom awqat-pre-notifications-for-times '(t t t t t t)
  "List of booleans indicating whether to send pre-notifications.
Used for each prayer time.  The list order corresponds to: (Fajr Sunrise
Dhuhr Asr Maghrib Isha)."
  :type '(repeat boolean)
  :group 'awqat)

(defcustom awqat-pre-notification-title "Upcoming Prayer"
  "Title for the pre-prayer notification."
  :type 'string
  :group 'awqat)

(defcustom awqat-pre-notification-message-format "%s will be in %d minutes"
  "Format string for pre-prayer notification message.
%s will be replaced with the prayer name.
%d will be replaced with minutes remaining."
  :type 'string
  :group 'awqat)

(defvar awqat--notification-timers nil
  "List of timers for upcoming notifications.")

(defvar awqat--pre-notification-timers nil
  "List of timers for upcoming pre-notifications.")

;;; Preconfigured presets

(defun awqat-use-angle-based-method ()
  "Set the calculation for Isha and Fajr to be angle based."
  (setq awqat-prayer-funs (list #'awqat--prayer-fajr
                                 #'awqat--prayer-sunrise
                                 #'awqat--prayer-dhuhr
                                 #'awqat--prayer-asr
                                 #'awqat--prayer-maghrib
                                 #'awqat--prayer-isha)))

(defun awqat-use-time-offset-method ()
  "Set the calculation for Isha and Fajr to be hours before/after based."
  (setq awqat-prayer-funs (list #'awqat--prayer-fajr-offset
                                 #'awqat--prayer-sunrise
                                 #'awqat--prayer-dhuhr
                                 #'awqat--prayer-asr
                                 #'awqat--prayer-maghrib
                                 #'awqat--prayer-isha-offset)))

(defun awqat-set-preset-diyanet ()
  "Set the calculation method to be similar to the Muslim Pro app."
  (setq awqat-prayer-funs (list #'awqat--prayer-fajr
                                 #'awqat--prayer-sunrise
                                 #'awqat--prayer-dhuhr
                                 #'awqat--prayer-asr
                                 #'awqat--prayer-maghrib
                                 #'awqat--prayer-isha))
  (setq awqat-fajr-angle -18.15)
  (setq awqat-isha-angle -13.94))

(defun awqat-set-preset-muslim-pro ()
  "Use the calculation method defined by the Muslim Pro app."
  (awqat--preset-with-angles -18.13 -16.3))

(defun awqat-set-preset-muslim-world-league ()
  "Use the calculation method defined by the Muslim World League."
  (awqat--preset-with-angles -18.0 -17.0))

(defun awqat-set-preset-karachi-university-of-islamic-sciences ()
  "Use calculation method by Karachi University of Islamic Sciences (KUIS)."
  (awqat--preset-with-angles -18.0 -18.0))

(defun awqat-set-preset-umm-al-qura ()
  "Use the calculation method defined by Umm al-Qura University, Makkah."
  (setq awqat-prayer-funs (list #'awqat--prayer-fajr
                                 #'awqat--prayer-sunrise
                                 #'awqat--prayer-dhuhr
                                 #'awqat--prayer-asr
                                 #'awqat--prayer-maghrib
                                 (lambda (d)
                                   (let ((maghrib-time (awqat--prayer-maghrib d))
                                         (ramadan-p (eq 9 (car (awqat--islamic-from-gregorian)))))
                                     (list (+ (car maghrib-time) (if ramadan-p 2.0 1.5))
                                           (cadr maghrib-time))))))
  (setq awqat-fajr-angle -18.5)
  (setq awqat-isha-angle nil))

(defun awqat-set-preset-egyptian-general-authority-of-survey ()
  "Use the calculation method defined by the Egyptian General Authority of Survey."
  (awqat--preset-with-angles -19.5 -17.5))

(defun awqat-set-preset-kuwait ()
  "Use the calculation method used in Kuwait."
  (awqat--preset-with-angles -18.0 -17.5))

(defun awqat-set-preset-institute-of-geophysics-university-of-tehran ()
  "Use calculation method by the Institute of Geophysics, University of Tehran."
  (setq awqat-prayer-funs (list #'awqat--prayer-fajr
                                 #'awqat--prayer-sunrise
                                 #'awqat--prayer-dhuhr
                                 #'awqat--prayer-asr
                                 (lambda (d)
                                   (list (caadr (awqat-sunrise-sunset-angle d -4.0))
                                         (awqat--timezone d)))
                                 #'awqat--prayer-isha))
  (setq awqat-fajr-angle -17.7)
  (setq awqat-isha-angle -14.0))

(defun awqat-set-preset-singapore ()
  "Use the calculation method defined by the Majlis Ugama Islam Singapura."
  (awqat--preset-with-angles -20.0 -18.0))

(defun awqat-set-preset-algeria ()
  "Use calculation method by Ministry of Religious Affairs and Wakfs, Algeria."
  (awqat--preset-with-angles -18.0 -17.0))

(defun awqat-set-preset-morocco ()
  "Use the calculation method used in Morocco."
  (awqat--preset-with-angles -18.0 -18.0))

(defun awqat-set-preset-taiwan ()
  "Use the calculation method used in Taiwan."
  (awqat--preset-with-angles -16.0 -19.0))

(defun awqat-set-preset-uae ()
  "Use the calculation method used in UAE."
  (awqat--preset-with-angles -18.2 -18.2))

(defun awqat-set-preset-jakim ()
  "Use calc method by Department of Islamic Development Malaysia (JAKIM)."
  (awqat--preset-with-angles -20.0 -18.0))

(defun awqat-set-preset-spiritual-administration-of-muslims-russia ()
  "Use calculation method by Spiritual Administration of Muslims, Russia (SAMR)."
  (awqat--preset-with-angles -16.0 -15.0))

(defun awqat-set-preset-french-muslims ()
  "Use calculation method by the French Muslims.
Former: Union des Organisations Islamiques de France."
  (awqat--preset-with-angles -12.0 -12.0))

(defun awqat-set-preset-grande-mosquee-de-paris ()
  "Use calculation method similar to one used by Grande Mosquée de Paris, Fr."
  (awqat--preset-with-angles -15.0 -13.0))

(defun awqat-set-preset-isna ()
  "Use calculation method by Islamic Society of North America (ISNA)."
  (awqat--preset-with-angles -15.0 -15.0))

(defun awqat-set-preset-midnight ()
  "Use the calculation method used in higher latitudes (Midnight method)."
  (setq awqat-prayer-funs (list #'awqat--prayer-fajr-midnight
                                 #'awqat--prayer-sunrise
                                 #'awqat--prayer-dhuhr
                                 #'awqat--prayer-asr
                                 #'awqat--prayer-maghrib
                                 #'awqat--prayer-isha-midnight)))

(defun awqat-set-preset-one-seventh-of-night ()
  "Use calculation method for higher latitudes (One-seventh of night method)."
  (setq awqat-prayer-funs (list #'awqat--prayer-fajr-one-seventh-of-night
                                 #'awqat--prayer-sunrise
                                 #'awqat--prayer-dhuhr
                                 #'awqat--prayer-asr
                                 #'awqat--prayer-maghrib
                                 #'awqat--prayer-isha-one-seventh-of-night)))

(defun awqat-set-preset-moonsighting-committee-worldwide  ()
  "Use calculation method defined by the Moonsighting Committee Worldwide (MCW).
This is a latitude and season aware method."
  (setq awqat-prayer-funs (list #'awqat--prayer-fajr-moonsighting
                                 #'awqat--prayer-sunrise
                                 #'awqat--prayer-dhuhr
                                 #'awqat--prayer-asr
                                 #'awqat--prayer-maghrib
                                 #'awqat--prayer-isha-moonsighting))
  (setq awqat-fajr-angle -18.0)
  (setq awqat-isha-angle -18.0))

;;; Presets helper functions

(defun awqat--preset-with-angles (fajr isha)
  "Use the standard angle method calculation with FAJR and ISHA angles."
  (setq awqat-prayer-funs (list #'awqat--prayer-fajr
                                 #'awqat--prayer-sunrise
                                 #'awqat--prayer-dhuhr
                                 #'awqat--prayer-asr
                                 #'awqat--prayer-maghrib
                                 #'awqat--prayer-isha))
  (setq awqat-fajr-angle fajr)
  (setq awqat-isha-angle isha))

;;; UI/Interactive functions and helpers.

(defun awqat--now ()
  "Return current time in hours."
  (let ((time-lst (decode-time)))
    (+ (decoded-time-hour time-lst)
       (/ (decoded-time-minute time-lst) 60.0))))

(defun awqat--today ()
  "Return today's date in the format (M D Y)."
  (let ((time-lst (decode-time)))
    (list (decoded-time-month time-lst)
          (decoded-time-day time-lst)
          (decoded-time-year time-lst))))

(defun awqat--tomorrow ()
  "Return tommorow's date in the format (M D Y)."
  (let* ((tomorrow (time-add nil (* 60 60 24)))
         (time-lst (decode-time tomorrow)))
    (list (decoded-time-month time-lst)
          (decoded-time-day time-lst)
          (decoded-time-year time-lst))))

(defun awqat-times-for-day ()
  "Calculate and display all prayer times for today."
  (interactive)
  (let* ((now (awqat--now))
         (todays-times (awqat--times-for-day))
         (ret-str "")
         (index 0)
         (next-prayer 0)
         (earliest-time)
         (time-remaining))
    (dolist (time todays-times ret-str)
      (setq ret-str (concat ret-str (awqat-pretty-time time) "  "))
      (when (and (not time-remaining) (< now (car time)))
        (setq time-remaining (- (car time) now)
              next-prayer index))
      (when (not earliest-time)
        (setq earliest-time time))
      (setq index (1+ index)))
    (when (not time-remaining)
      (setq time-remaining (mod (- (+ 24.0 (car earliest-time)) now) 24)))
    (message "%s-- %s in %s"
             ret-str
             (nth next-prayer awqat--prayer-names)
             (propertize (format "%dh%02dm"
                                 (floor time-remaining)
                                 (* 60 (- time-remaining (floor time-remaining))))
                         'face (if (< time-remaining 0.5) '(:foreground "red") '())))))

(defun awqat--times-for-day (&optional day)
  "Return a list of all the prayer times for a given DAY.
Or for today if no DAY is provided."
  (let ((day (or day (awqat--today)))
        (times '()))
    (dolist (time-idx (number-sequence 0 (1- (length awqat-prayer-funs))))
      (let ((time (awqat--prayer-time day time-idx)))
        (push time times)))
    (nreverse times)))

(defun awqat--next-time ()
  "Return the next time comming up."
  (let* ((now (awqat--now))
         (times (seq-map-indexed (lambda (time idx)
                                   (append time (list idx)))
                                 (awqat--times-for-day)))
         (after-times (seq-filter (lambda (time) (> (car time) now)) times)))
    (if after-times
        (car after-times)
      (append (car (awqat--times-for-day (awqat--tomorrow))) (list 0)))))

(with-suppressed-warnings ((lexical date))
  (defvar date))

(defun awqat--diary-prayer (prayer)
  "Display prayer time for PRAYER (index from 0 to 5)."
  (if (or org-agenda-show-future-repeats (time-equal-p (awqat--today) date))
      (let ((prayer-time (car (awqat--prayer-time date prayer)))
            (prayer-name (nth prayer awqat--prayer-names)))
        (concat prayer-name " " (solar-time-string prayer-time nil)))))

(defun awqat-diary-fajr ()
  "Diary prayer time for Fajr."
  (awqat--diary-prayer 0))

(defun awqat-diary-sunrise ()
  "Display Sunrise time."
  (awqat--diary-prayer 1))

(defun awqat-diary-dhuhr ()
  "Display Dhuhr time."
  (awqat--diary-prayer 2))

(defun awqat-diary-asr ()
  "Display Asr time."
  (awqat--diary-prayer 3))

(defun awqat-diary-maghrib ()
  "Display maghrib time."
  (awqat--diary-prayer 4))

(defun awqat-diary-isha ()
  "Display Isha time."
  (awqat--diary-prayer 5))

;;; Prayer Calculations ------------------------------------------------------------------

(defun awqat--prayer-time (d prayer)
  "Calculate a time for PRAYER idx on given date D, applying safety offsets."
  (let* ((offset (nth prayer awqat-prayer-safety-offsets))
         (fun (nth prayer awqat-prayer-funs))
         (time (apply fun (list d))))
    (list (+ (car time) (/ offset 60.0)) (cadr time))))

(defun awqat--prayer-fajr-from-sunrise (d offset)
  "Calculate the Fajr time for a given date D, based on a OFFSET from sunrise."
  (let ((sunrise-time (awqat--prayer-sunrise d)))
    (list (- (car sunrise-time) offset)
          (cadr sunrise-time))))

(defun awqat--prayer-isha-from-sunset (d offset)
  "Calculate Isha time for given date D, based on OFFSET from sunset (Maghrib)."
  (let ((maghrib-time (awqat--prayer-maghrib d)))
    (list (+ (car maghrib-time) offset)
          (cadr maghrib-time))))

(defun awqat--moonsighting-get-offset (type)
  "Get the offset in hours for TYPE.
TYPE being a valid symbol for the `awqat-isha-moonsighting-method' variable.
Used by the Moonsighting Committee Worldwide method."
  (let* ((consts (awqat--moonsighting-constants type))
         (a (nth 0 consts))
         (b (nth 1 consts))
         (c (nth 2 consts))
         (d (nth 3 consts))
         (dyy (awqat--days-since-winter-solstice)))
    (/ (cond ((< dyy 91)
              (+ a (* (/ (- b a) 91.0) dyy)))
             ((< dyy 137)
              (+ b (* (/ (- c b) 46.0) (- dyy 91))))
             ((< dyy 183)
              (+ c (* (/ (- d c) 46.0) (- dyy 137))))
             ((< dyy 229)
              (+ d (* (/ (- c d) 46.0) (- dyy 183))))
             ((< dyy 275)
              (+ c (* (/ (- b c) 46.0) (- dyy 229))))
             ((>= dyy 275)
              (+ b (* (/ (- a b) 91.0) (- dyy 275)))))
       60.0)))

(defun awqat--days-since-winter-solstice ()
  "Return the days count since the last winter solstice from today."
  (let* ((today (awqat--today))
         (date-zero-month (if (> calendar-latitude 0.0) 12 06))
         (curr-year (calendar-extract-year today))
         (prev-year-p (calendar-date-compare
                       (list today)
                       (list (list date-zero-month 21 curr-year))))
         (date-zero (list date-zero-month 21 (if prev-year-p (1- curr-year) curr-year))))
    (- (calendar-absolute-from-gregorian (awqat--today))
       (calendar-absolute-from-gregorian date-zero))))

(defun awqat--moonsighting-constants (type)
  "Return a list of (a b c d) constants for calculation TYPE, for a given LATITUDE."
  ;; CTE = alpha + beta / 55.0 * abs(latitude)
  (mapcar (lambda (ab) (+ (car ab) (* (/ (cadr ab) 55.0) (abs calendar-latitude))))
          (pcase type
            ('subh-sadiq
             '((75.0 28.65) (75.0 19.44) (75.0 32.74) (75.0 48.10)))
            ('shafaq-ahmer
             '((62.0 17.40) (62.0 -7.16) (62.0 05.12) (62.0 19.44)))
            ('shafaq-abyad
             '((75.0 25.60) (75.0 07.16) (75.0 36.84) (75.0 81.84)))
            ('shafaq
             '((75.0 25.60) (75.0 02.05) (75.0 -9.21) (75.0 06.14))))))

;; The following functions can be put into the awqat-prayer-funs list.
;; Fajr

(defun awqat--prayer-fajr (d)
  "Calculate the time of fajr for date D using standard angle method."
  (car (awqat-sunrise-sunset-angle d awqat-fajr-angle)))

(defun awqat--prayer-fajr-diyanet (d)
  "Calculate the time of fajr for date D using third-portion if lat > 45."
  (if (< calendar-latitude 45.0)
      (awqat--prayer-fajr d)
    (let ((third-portion (awqat--third-portion-calc d awqat-fajr-angle))
          (sunrise (awqat--sunrise d))
          (timezone (awqat--timezone d)))
      (list (if (> third-portion 1.3333)
                (- sunrise 1.3333)
              (+ sunrise third-portion))
            timezone))))

(defun awqat--prayer-fajr-angle (d)
  "Calculate the time of fajr for date D using angle method if necessary."
  (if (awqat--use-angle-method-p d)
      (let ((offset (awqat--angle-approx-offset d awqat-fajr-angle))
            (timezone (awqat--timezone d))
            (sunrise (awqat--sunrise d)))
        (list (- sunrise offset) timezone))
    (awqat--prayer-fajr d)))

(defun awqat--prayer-fajr-offset (d)
  "Calculate the time of fajr based on fixed time for given date D."
  (let ((sunrise (awqat--sunset d))
        (timezone (awqat--sunset d)))
    (list (- sunrise awqat-fajr-before-sunrise-offset)
          timezone)))

(defun awqat--prayer-fajr-one-seventh-of-night (d)
  "Calculate the time of fajr for a given date D.
The one-seventh of night method is an approximation used in
higher latitudes during the abnormal period."
  (when (< -48.5 calendar-latitude 48.5)
    (warn "This method should only be used in latitudes beyond 48.5°N and 48.5°S."))

  (let ((offset (/ (awqat-duration-of-night d) 7.0)))
    (awqat--prayer-fajr-from-sunrise d offset)))

(defun awqat--prayer-fajr-midnight (d)
  "Calculate the time of fajr for a given date D.
The midnight method is an approximation used in higher latitudes
during the abnormal period.  It defines the Isha and Fajr times
to be the same, starting at the midnight between sunset and
sunrise."
  (when (< -48.5 calendar-latitude 48.5)
    (warn "This method should only be used in latitudes beyond 48.5°N and 48.5°S."))

  (let ((offset (/ (awqat-duration-of-night d) 2.0)))
    (awqat--prayer-fajr-from-sunrise d offset)))

(defun awqat--prayer-fajr-moonsighting (d)
  "Calculate the time of Fajr for a given date D.
The Moonsighting Committee Worldwide (MCW) method is a latitude
and season aware method.  It takes into account placed in higher
latitudes, up to 60°N/S."
  (cond ((< (abs calendar-latitude) 55.0)
         ;; From equator to 55°, the 18° depression angle calculations are compared with the values
         ;; given by the functions of latitude and seasons and most favorable values are used, which
         ;; means; for Fajr, the later of the two and for Isha the earlier of the two.
         (let* ((offset (awqat--moonsighting-get-offset 'subh-sadiq))
                (fajr-18 (car (awqat-sunrise-sunset-angle d -18.0))))
           (list (max (car fajr-18)
                      (- (car (awqat--prayer-sunrise d)) offset))
                 (awqat--timezone d))))

        ((and (<= 55.0 (abs calendar-latitude)) (< (abs calendar-latitude) 60.0))
         (awqat--prayer-fajr-one-seventh-of-night d))
        (t (warn "Latitudes beyond 60°N/S, hardship prevails and beyond 65°,
the sun does not set/rise for a number of days every year."))))

;; Sunrise

(defun awqat--prayer-sunrise (d)
  "Calculate the time of the sunrise on a given date D."
  (list (awqat--sunrise d)
        (awqat--timezone d)))

;; Dhuhr

(defun awqat--prayer-dhuhr (d)
  "Calculate the prayer time for dhuhr on a given date D."
  (awqat-solar-noon d))

;; Asr

(defun awqat--prayer-asr (d)
  "Calculate the time of asr on date D.

If `awqat-asr-hanafi' is non-nil, use double the length of noon shadow."
  (let* ((s (awqat-length-of-shadow-at-noon d))
         (l (+ (if awqat-asr-hanafi 2 1) s))
         (h (awqat-rad-to-deg (atan (/ 1 l)))))
    (cadr (awqat-sunrise-sunset-angle d h))))

;; Maghrib

(defun awqat--prayer-maghrib (d)
  "Calculate the time of maghrib on date D."
  (list (awqat--sunset d)
        (awqat--timezone d)))

;; Isha

(defun awqat--prayer-isha (d)
  "Calculate the time of isha on date D using the standard angle method."
  (cadr (awqat-sunrise-sunset-angle d awqat-isha-angle)))

(defun awqat--prayer-isha-diyanet (d)
  "Calculate the time of fajr for date D using third-portion if lat > 45."
  (if (< calendar-latitude 45.0)
      (awqat--prayer-isha d)
    (let ((third-portion (awqat--third-portion-calc d awqat-isha-angle))
          (sunset (awqat--sunset d))
          (timezone (awqat--timezone d)))
      (list (if (> third-portion 1.3333)
                (+ sunset 1.3333)
              (+ third-portion sunset))
            timezone))))

(defun awqat--prayer-isha-angle (d)
  "Calculate the time of fajr for date D using angle method if necessary."
  (if (awqat--use-angle-method-p d)
      (let ((offset (awqat--angle-approx-offset d awqat-isha-angle))
            (timezone (awqat--timezone d))
            (sunset (awqat--sunrise d)))
        (list (+ sunset offset) timezone))
    (awqat--prayer-isha d)))

(defun awqat--prayer-isha-offset (d)
  "Calculate the time of fajr based on fixed time for given date D."
  (let ((sunset (awqat--sunset d))
        (timezone (awqat--sunset d)))
    (list (+ sunset awqat-isha-after-sunset-offset)
          timezone)))

(defun awqat--prayer-isha-midnight (d)
  "Calculate the time of isha for a given date D.
The midnight method is an approximation used in higher latitudes
during the abnormal period.  It defines the Isha and Fajr times
to be the same, starting at the midnight between sunset and
sunrise."
  (awqat--prayer-fajr-midnight d))

(defun awqat--prayer-isha-one-seventh-of-night (d)
  "Return the Isha time for a given date D.
The one-seventh of night method is an approximation used in
higher latitudes during the abnormal period."
  (when (< -48.5 calendar-latitude 48.5)
    (warn "This method should only be used in latitudes beyond 48.5°N and 48.5°S."))

  (let ((offset (/ (awqat-duration-of-night d) 7.0)))
    (awqat--prayer-isha-from-sunset d offset)))

(defun awqat--prayer-isha-moonsighting (d)
  "Calculate the time of Isha for a given date D.
The Moonsighting Committee Worldwide (MCW) method is a latitude
and season aware method.  It takes into account placed in higher
latitudes, up to 60°N/S."
  (cond ((< (abs calendar-latitude) 55.0)
         ;; From equator to 55°, the 18° depression angle calculations are compared with the values
         ;; given by the functions of latitude and seasons and most favorable values are used, which
         ;; means; for Fajr, the later of the two and for Isha the earlier of the two.
         (let* ((offset (awqat--moonsighting-get-offset awqat-isha-moonsighting-method))
                (isha-18 (cadr (awqat-sunrise-sunset-angle d -18.0))))
           (list (awqat--isha-time-min
                  d (car isha-18) (+ (car (awqat--prayer-maghrib d)) offset))
                 (awqat--timezone d))))
        ((and (<= 55.0 (abs calendar-latitude)) (< (abs calendar-latitude) 60.0))
         (awqat--prayer-isha-one-seventh-of-night d))
        (t (warn "Latitudes beyond 60°N/S, hardship prevails and beyond 65°,
the sun does not set/rise for a number of days every year."))))

;;; Time Calculations --------------------------------------------------------------------

(defun awqat--angle-approx-offset (d angle)
  "Calculate the time of isha for date D at with ANGLE using andle method.

This method calculates the time based on a portion of angle/60 of the night."
  (let* ((night-duration (awqat-duration-of-night d)))
    (/ (* (abs angle) night-duration) 60)))

(defun awqat--third-portion-calc (d angle)
  "Calculate the time of isha for date D with ANGLE using third portion method.
Calculate fajr if IS-ISHA is false.

This method is considered only if calendar latitude is greater than 45deg."
  (let* ((sunset (awqat--sunset d))
         (fecri-sadik (caadr (awqat-sunrise-sunset-angle d angle))))
    (/ (mod (- (+ fecri-sadik 24.0) sunset) 24.0) 3)))

(defun awqat--timezone (d)
  "Return the timezone used to calculate times on date D."
  (cadar (awqat-sunrise-sunset d)))

(defun awqat--sunrise (d)
  "Calculate the sunrise for a given date D."
  (caar (awqat-sunrise-sunset d)))

(defun awqat--sunset (d)
  "Calculate the sunset for a given date D."
  (caadr (awqat-sunrise-sunset d)))

(defun awqat--use-angle-method-p (d)
  "Determine if on date D, the angle method for isha/fajr should be used."
  (let* ((isha (caadr (awqat-sunrise-sunset-angle d awqat-isha-angle)))
         (fajr (caar (awqat-sunrise-sunset-angle d awqat-fajr-angle))))
    (if (or (not isha) (not fajr))
        t
      (let* ((isha-offset (awqat--angle-approx-offset d awqat-isha-angle))
             (sunset (awqat--sunset d))
             (isha-angle (+ sunset isha-offset))
             (fajr-offset (awqat--angle-approx-offset d awqat-fajr-angle))
             (sunrise (awqat--sunrise d))
             (fajr-angle (- sunrise fajr-offset))
             (isha-fajr-diff (mod (- (+ 24.0 fajr) isha) 24)) ;
             (isha-fajr-angle-diff (- (+ 24.0 fajr-angle) isha-angle))) ;
        (> isha-fajr-angle-diff isha-fajr-diff)))))

(defun awqat--isha-time-min (d time1 time2)
  "Works like `min', used to compare isha TIME1 and TIME2 for date D.
This takes into account the midnight comparaison, so 23.0 is before 00.2.
Do not use for times other than isha."
  (let* ((sunset (awqat--sunset d))
         (t1 (if (< time1 sunset) (+ 24.0 time1) time1))
         (t2 (if (< time2 sunset) (+ 24.0 time2) time2)))
    (if (< t1 t2) time1 time2)))

;;; Astronomical Calculations -----------------------------------------------------------

(defun awqat-sunrise-sunset (d)
  "Calculate the times of maghrib and imsak for date D."
  (awqat-sunrise-sunset-angle d awqat-sunrise-sunset-angle))

(defun awqat-height-of-sun-at-noon (d)
  "Calculates the height of at solar noon on date D."
  (let* ((exact-local-noon (solar-exact-local-noon d))
         (t0 (solar-julian-ut-centuries (car exact-local-noon)))
         (ut (cadr exact-local-noon))

         (hnoon (solar-horizontal-coordinates (list t0 ut)
                                              (calendar-latitude)
                                              (calendar-longitude) t)))
    hnoon))

;; modified from solar.el
(defun awqat-sunrise-sunset-angle (d angle)
  "Calculate sunrise/sunset on date D with ANGLE above horizon.
Example of date: (7 22 2019)"
  (let ((res (let* ((exact-local-noon (solar-exact-local-noon d))
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
                    (adj-rise (if rise-time (dst-adjust-time d rise-time)))
                    (set-time (cadr rise-set))
                    (adj-set (if set-time (dst-adjust-time d set-time)))
                    (length (nth 2 rise-set)))
               (list
                (and rise-time (cdr adj-rise))
                (and set-time (cdr adj-set))
                (solar-daylight length)))))
    (if (car res)
        res
      (let ((solar-noon (awqat-solar-noon d)))
        (list solar-noon solar-noon)))))

(defun awqat-duration-of-night (d)
  "Return the duration from sunset to sunset for a given date D.
Adapted from `solar-sunrise-and-sunset'."
  (let* ((rise-set (awqat-sunrise-sunset d))
         (rise-time (caar rise-set))
         (set-time (caadr rise-set)))
    (if (not (and rise-time set-time))
        (if (or (and (> (calendar-latitude) 0)
                     solar-northern-spring-or-summer-season)
                (and (< (calendar-latitude) 0)
                     (not solar-northern-spring-or-summer-season)))
            0 ; no night
          24) ; no day
      (- (+ rise-time 24.0) set-time))))

(defun awqat-solar-noon (d)
  "Calculate the time of Zuhr on date D."
  (let* ((exact-local-noon (solar-exact-local-noon d))
         (midday-time (dst-adjust-time d (cadr exact-local-noon)))
         (base-time (cadr midday-time)))
    (setcar (cdr midday-time) (+ base-time (/ calendar-time-zone 60.0)))
    (cdr midday-time)))

(defun awqat-length-of-shadow-at-noon (d)
  "Calculates the relative length of an objects shadow at solar noon on date D."
  (let ((hn (cadr (awqat-height-of-sun-at-noon d))))
    (/ 1 (tan (awqat-deg-to-rad hn)))))

(defun awqat-rad-to-deg (x)
  "Convert X from radians to degrees."
  (/ (* x 180) float-pi))

(defun awqat-deg-to-rad (x)
  "Convert X from radians to degrees."
  (/ (* x float-pi) 180))

(defun awqat-pretty-time (time &optional flag)
  "Format TIME to human readable form and colored if FLAG is not nil.

If FLAG is the symbol `skip' then return empty string."
  (if (eq flag 'skip)
      ""
    (let ((pretty-time (if time (apply #'solar-time-string time) "---")))
      (if flag
          (propertize pretty-time 'face '(:background "yellow"))
        pretty-time))))

;;; Notification functions

(defcustom awqat-alert-style nil
  "The alert style to use for awqat notifications.
If nil, use the default alert style.  See `alert-styles' for options."
  :type '(choice (const :tag "Default" nil)
                 (symbol :tag "Alert style"))
  :group 'awqat)

(defun awqat--send-notification (title message)
  "Send a desktop notification with TITLE and MESSAGE using the alert package."
  (let ((alert-default-style (or awqat-alert-style alert-default-style)))
    (alert message
           :title title
           :category 'awqat)))


(defun awqat--schedule-prayer-notifications ()
  "Schedule notifications for today's prayer times."
  (when awqat-notifications-enabled
    ;; Cancel any existing timers
    (dolist (timer awqat--notification-timers)
      (when (timerp timer)
        (cancel-timer timer)))
    (setq awqat--notification-timers nil)

    ;; Schedule new timers for prayer times
    (let ((times (awqat--times-for-day))
          (now (float-time))
          (notification-timers nil))
      (dotimes (idx (length times))
        (when (nth idx awqat-notifications-for-times)
          (let* ((time (nth idx times))
                 (hour (floor (car time)))
                 (minute (floor (* 60 (- (car time) hour))))
                 (prayer-name (nth idx awqat--prayer-names))
                 (current-time (decode-time))
                 (notification-time
                  (encode-time 0 minute hour
                              (decoded-time-day current-time)
                              (decoded-time-month current-time)
                              (decoded-time-year current-time))))
            (when (> (float-time notification-time) now)
              (let ((timer (run-at-time notification-time nil
                                        #'awqat--send-notification
                                        awqat-notification-title
                                        (format awqat-notification-message-format prayer-name))))
                (push timer notification-timers)))))
        (setq awqat--notification-timers notification-timers)))))

(defun awqat--schedule-pre-notifications ()
  "Schedule pre-notifications for today's prayer times."
  (when (and awqat-notifications-enabled awqat-pre-notifications-enabled)
    ;; Cancel any existing timers
    (dolist (timer awqat--pre-notification-timers)
      (when (timerp timer)
        (cancel-timer timer)))
    (setq awqat--pre-notification-timers nil)

    ;; Schedule new timers for pre-prayer notifications
    (let ((times (awqat--times-for-day))
          (now (float-time))
          (pre-notification-timers nil))
      (dotimes (idx (length times))
        (when (nth idx awqat-pre-notifications-for-times)
          (let* ((time (nth idx times))
                 (hour (floor (car time)))
                 (minute (floor (* 60 (- (car time) hour))))
                 (pre-minute (- minute awqat-pre-notification-minutes))
                 (pre-hour (if (< pre-minute 0)
                              (1- hour)
                            hour))
                 (pre-minute (if (< pre-minute 0)
                                (+ 60 pre-minute)
                              pre-minute))
                 (prayer-name (nth idx awqat--prayer-names))
                 (current-time (decode-time))
                 (notification-time
                  (encode-time 0 pre-minute pre-hour
                              (decoded-time-day current-time)
                              (decoded-time-month current-time)
                              (decoded-time-year current-time))))
            (when (> (float-time notification-time) now)
              (let ((timer (run-at-time notification-time nil
                                        #'awqat--send-notification
                                        awqat-pre-notification-title
                                        (format awqat-pre-notification-message-format
                                                prayer-name
                                                awqat-pre-notification-minutes))))
                (push timer pre-notification-timers)))))
        (setq awqat--pre-notification-timers pre-notification-timers)))))

;;; Misc

(defun awqat--islamic-from-gregorian (&optional d)
  "Get Islamic Hijri date from a Gregorian date D."
  (calendar-islamic-from-absolute (calendar-absolute-from-gregorian
                                   (or d (calendar-current-date)))))

;;; awqat-display-prayer-time-mode

(defconst awqat-mode-line-string nil)
(defvar awqat-update-timer nil)

(defcustom awqat-update-interval 5.0
  "Interval, in seconds, of prayer times in the mode-line."
  :type 'float
  :group 'awqat)

(defcustom awqat-warning-duration 0.75
  "Duration to next prayer, for `awqat-display-prayer-time-mode' to show a warning."
  :type 'float
  :group 'awqat)

(defcustom awqat-danger-duration 0.33
  "Duration to next prayer, for `awqat-display-prayer-time-mode' to show a danger."
  :type 'float
  :group 'awqat)

(defcustom awqat-mode-line-format "﴾${hours}h${minutes}m>${prayer}﴿ "
  "Formatting string to use in mode-line.
Use ${prayer} to refer to the next prayer name,
and ${hours} and ${minutes} to refer to the remaining time."
  :type 'str
  :group 'awqat)

(defface awqat-warning-face
  '((t (:inherit warning)))
  "Face used to show a somewhat short duration of time."
  :group 'awqat)

(defface awqat-danger-face
  '((t (:inherit error)))
  "Face used to show a very short duration of time."
  :group 'awqat)

(defun awqat--get-face-from-duration (duration)
  "Return face to use for display based on remaining DURATION.
DURATION should be a floating-point number representing number of hours."
  (cond
   ((< duration awqat-danger-duration) 'awqat-danger-face)
   ((< duration awqat-warning-duration) 'awqat-warning-face)
   (t nil)))

(defun awqat-update ()
  "Update mode-line display for remaining prayer time."
  (let ((next-time (awqat--next-time)))
    (seq-let (time _ idx) next-time
      (let* ((name (nth idx awqat--prayer-names))
             (time-remaining (mod (+ (- time (awqat--now)) 24.0) 24.0))
             (h (floor time-remaining))
             (m (floor (* (mod time-remaining 1) 60)))
             (face (awqat--get-face-from-duration time-remaining))
             (message (s-format awqat-mode-line-format 'aget (list (cons "prayer" name) (cons "hours" h) (cons "minutes" m))))
             (len (length message)))
        (add-face-text-property 0 len face t message)
        (setq awqat-mode-line-string message)))
    (force-mode-line-update t)))

(defun awqat-update-handler ()
  "Update handler for mode-line display of prayer time."
  (awqat-update)
  (sit-for 0))

;;;###autoload
(define-minor-mode awqat-display-prayer-time-mode
  "Toggle prayer time status display in mode line."
  :global t
  (setq awqat-mode-line-string "")
  (or global-mode-string (setq global-mode-string '("")))
  (and awqat-update-timer (cancel-timer awqat-update-timer))
  (if (not awqat-display-prayer-time-mode)
      (setq global-mode-string
            (delq 'awqat-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'awqat-mode-line-string t)
    (setq awqat-update-timer (run-at-time nil awqat-update-interval #'awqat-update-handler))
    (awqat-update)))

;;; Adhan Mode

(defcustom awqat-audio-player "ffplay"
  "Music player used to play sounds.
Possible values are \"ffplay\", \"aplay\", or \"afplay\"."
  :group 'awqat
  :type '(choice (const "ffplay")
                 (const "afplay")
                 (const "aplay")))

(defun awqat--play-sound (sound-file)
  "Create and return a process to play SOUND-FILE.
The program to use is specified in the variable `awqat-audio-player'."
  (let ((cmd-args
         (pcase (file-name-base awqat-audio-player)
           ("ffplay"
            (list awqat-audio-player "-nodisp" "-autoexit" sound-file))
           (_ (list awqat-audio-player sound-file)))))
    (apply #'start-process (append '("Awqat adhan sound"
                                     "*awqat-sound-process*")
                                   cmd-args))))

(defcustom awqat-play-adhan-for-times '(t nil t t t t)
  "List, corresponding to elements of `awqat-prayer-funs'.
Used to determine if adhan should be played.  For example, for the
default value of `awqat-prayer-funs', setting this variable to
the value (t nil t t t t) would result it all sounds playing
except ishak.  A non-nil value indicates that the adhan should
play.  If the value is a string, it is interpreted as a specific
file to play for the specified time."
  :group 'awqat
  :type 'list)
(defalias 'awqat--play-adhan-for-times #'awqat-play-adhan-for-times)

(defcustom awqat-adhan-file nil
  "Path to the sound file to play when the prayer time is reached."
  :type '(file :must-match t)
  :group 'awqat)
(defalias 'awqat--adhan-file #'awqat-adhan-file)

(defvar awqat--adhan-process nil
  "The process playing the current sound.  Used to stop the sound.")

(defvar awqat--next-adhan-timer nil
  "Timer for the next adhan to be played.")

(defun awqat--play-adhan (&optional sound-file)
  "Play the WAV SOUND-FILE using Emacs' `play-sound-file' function.
If no SOUND-FILE is provided, use `awqat-adhan-file'."
  (let ((sound-file (or sound-file awqat-adhan-file)))
    (if sound-file
        (setq awqat--adhan-process (awqat--play-sound (expand-file-name sound-file)))
      (message "Adhan playing (no sound available to play)"))
    ;; Send notification for adhan if enabled
    (when awqat-notifications-enabled
      (let* ((next-time (awqat--next-time))
             (idx (nth 2 next-time))
             (prayer-name (nth idx awqat--prayer-names)))
        (awqat--send-notification
         awqat-notification-title
         (format awqat-notification-message-format prayer-name))))
    (awqat--adhan-update)))

(defun awqat--adhan-update ()
  "Schedule the next adhan to play."
  (seq-let (time _ idx) (awqat--next-time)
    (let* ((hours-remaining (mod (+ (- time (awqat--now)) 24.0) 24.0))
           (seconds-remaining (ceiling (* hours-remaining 60 60)))
           (special-sound (nth idx awqat-play-adhan-for-times)))
      (if (< seconds-remaining 10) ;; to prevent duplicate runs
          (run-at-time 60 nil #'awqat--adhan-update)
        (setq awqat--next-adhan-timer
              (if (stringp special-sound)
                  (run-at-time seconds-remaining nil (lambda ()
                                                       (awqat--play-adhan special-sound)))
                (run-at-time seconds-remaining nil #'awqat--play-adhan)))))))

(defun awqat--stop-adhan ()
  "Stop the currently playing sound."
  (interactive)
  (when awqat--adhan-process
    (delete-process awqat--adhan-process)
    (setq awqat--adhan-process nil)
    (message "Sound stopped.")))

;;;###autoload
(define-minor-mode awqat-adhan-mode
  "Toggle the playing of the adhan for each time."
  :global t
  (and awqat--next-adhan-timer (cancel-timer awqat--next-adhan-timer))
  (awqat--stop-adhan)
  (when awqat-adhan-mode
    (awqat--adhan-update)))

;;;###autoload
(define-minor-mode awqat-notification-mode
  "Toggle sending desktop notifications for prayer times."
  :global t
  :group 'awqat
  (dolist (timer awqat--notification-timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (dolist (timer awqat--pre-notification-timers)
    (when (timerp timer)
      (cancel-timer timer)))
  (setq awqat--notification-timers nil)
  (setq awqat--pre-notification-timers nil)

  (when awqat-notification-mode
    (awqat--schedule-prayer-notifications)
    (awqat--schedule-pre-notifications)
    ;; Reschedule notifications at midnight
    (run-at-time "00:01" (* 24 60 60) #'awqat--schedule-prayer-notifications)
    (when awqat-pre-notifications-enabled
      (run-at-time "00:01" (* 24 60 60) #'awqat--schedule-pre-notifications))))

(provide 'awqat)
;;; awqat.el ends here
