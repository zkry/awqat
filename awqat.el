;;; awqat.el --- Islamic prayer times -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2026 Zachary Romero

;; Package-Requires: ((emacs "28.1") (alert "1.2"))
;; Package-Version: 20250131.162501
;; Author: Zachary Romero <zacromero@posteo.net>
;; Contributor: Abdelhak Bougouffa <abougouffa@fedoraproject.org>
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

(defcustom awqat-maghrib-angle nil
  "The Maghrib zenith angle offset (in degrees) below horizon.

The value of this variable can be used to override
`awqat-sunrise-sunset-angle'. In most cases, this don't need to be set.
Only some specific methods, mainly Shia ones, uses different angles for
Maghrib."
  :type '(choice float (const nil))
  :group 'awqat)

(defcustom awqat-high-latitudes-adjustment-method 'angle-based
  "The adjustment method to use for high latitudes (> 48.5°).

It can be `one-seventh-of-night', `midnight' or `angle-based' or nil for
no adjustment."
  :group 'awqat
  :type '(choice (const one-seventh-of-night)
                 (const one-third-of-night)
                 (const midnight)
                 (const angle-based)
                 (const nil)))

(defcustom awqat-high-latitudes-adjust-maghrib nil
  "Apply high latitudes adjustment for Maghrib time.

Not very noticeable unless the Maghrib angle is quite important (greater
than the default -0.833)."
  :group 'awqat
  :type 'boolean)

(defcustom awqat-high-latitudes-adjustment-max-latitude 48.5
  "The latitude above which the high latitudes adjustment is applied.

Set to t to always correct, and to nil to not correct.

The `when-undefined' option is reserved for future use."
  :group 'awqat
  :type '(choice float (const t) (const nil) (const when-undefined)))

(defcustom awqat-elevation 0.0
  "The elevation above sea level in meters, at the user location.

If the observation point is at height H meters above sea level, the
depression angle of the horizon is increased by approximately:

  0.0347° * sqrt(H)

When set to non-zero, this will be used to adjust the sunrise/sunset
angles."
  :group 'awqat
  :type 'float)

(defcustom awqat-fajr-before-sunrise-offset 1.81
  "The Fajr time offset (in hours) before sunrise.

This is applicable only when calculating Fajr offset-based approaches."
  :type 'float
  :group 'awqat)

(defcustom awqat-maghrib-after-sunset-offset (/ 0.5 60.0) ; 30s
  "The Maghrib time offset (in hours) after sunset.

This is applicable only when calculating Maghrib offset-based approaches."
  :type 'float
  :group 'awqat)

(defcustom awqat-isha-after-sunset-offset 1.333
  "The Isha time offset (in hours) after sunset.

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

(defcustom awqat-asr-hanafi nil
  "Use the Hanafi jurisprudence (al-Fiqh al-Hanafi) for Asr time.

Default value is nil, corresponding to the majority opinion (al-Jomhor),
including the Maliki, Shafii, and Hambali schools of thought."
  :type 'boolean
  :group 'awqat)

(defcustom awqat-prayer-safety-offsets '(-3.0 0.0 3.0 3.0 3.0 3.0)
  "The offset in minutes applied for the six times.

It is recommended to set values between 3 and 5 minutes to account for
the uncertainty.

List ordered as: (Fajr Sunrise Dhuhr Asr Maghrib Isha)."
  :type '(list float)
  :group 'awqat)

(defcustom awqat-prayer-funs '(awqat--prayer-fajr
                               awqat--prayer-sunrise
                               awqat--prayer-dhuhr
                               awqat--prayer-asr
                               awqat--prayer-maghrib
                               awqat--prayer-isha)
  "The functions used to calculate each time, a list of six elements."
  :type '(list function)
  :group 'awqat)

(defvar awqat--prayer-names '("Fajr" "Sunrise" "Dhuhr" "Asr" "Maghrib" "Isha")
  "Names of the six times.")

;;; Notification settings
(defcustom awqat-notifications-enabled t
  "Whether to send desktop notifications for prayer times."
  :type 'boolean
  :group 'awqat)

(defcustom awqat-notifications-for-times '(t nil t t t t)
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

(defcustom awqat-pre-notifications-for-times '(t nil t t t t)
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

(defconst awqat-sunrise-sunset-angle -0.833
  "The sunrise/sunset zenith angle offset below horizon.

Used to determine the sunrise and sunset (Maghrib).
A zero value corresponds to the sun being at zenith=90°,
which means that the sun circle is still visible.
The apparent radius of the sun at the horizon is 16 arcminutes,
and the average refraction is known to be 34 arcminutes,
which gives an offset of 50 arcminutes, hence the 0.833° value.")

;;; Obsolete definition, to be removed in future releases

;; TODO: Remove after some time
(defun awqat-set-preset-midnight () (awqat-set-preset-high-latitudes 'midnight))
(defun awqat-set-preset-one-seventh-of-night () (awqat-set-preset-high-latitudes 'one-seventh-of-night))
(make-obsolete 'awqat-set-preset-midnight 'awqat-set-preset-high-latitudes "v1.0.0")
(make-obsolete 'awqat-set-preset-one-seventh-of-night 'awqat-set-preset-high-latitudes "v1.0.0")
(define-obsolete-function-alias 'awqat-duration-of-night 'awqat-night-duration "1.0.0")
(define-obsolete-function-alias 'awqat--stop-adhan 'awqat-stop-adhan "0.3.0")
(make-obsolete-variable 'awqat--prayer-funs 'awqat-prayer-funs "1.0.0")
(make-obsolete-variable 'awqat--adhan-file 'awqat-adhan-file "1.0.0")
(make-obsolete-variable 'awqat--play-adhan-for-times 'awqat-play-adhan-for-times "1.0.0")

;;; Preconfigured presets

(defun awqat-use-angle-based-method ()
  "Set the calculation for Isha and Fajr to be angle based."
  (setq awqat-prayer-funs '(awqat--prayer-fajr
                            awqat--prayer-sunrise
                            awqat--prayer-dhuhr
                            awqat--prayer-asr
                            awqat--prayer-maghrib
                            awqat--prayer-isha)))

(defun awqat-use-time-offset-method ()
  "Set the calculation for Isha and Fajr to be hours before/after based."
  (setq awqat-prayer-funs '(awqat--prayer-fajr-offset
                            awqat--prayer-sunrise
                            awqat--prayer-dhuhr
                            awqat--prayer-asr
                            awqat--prayer-maghrib
                            awqat--prayer-isha-offset)))

(defun awqat-set-preset-diyanet ()
  "Set the calculation method defined by Diyanet İşleri Başkanlığı, Turkey.

This preset sets some non-standard options, including a -2.0° angle for
sunrise/sunset and safety offsets for Dhuhr and Asr."
  (awqat--preset-with-angles -18.0 -17.0 -2.0)
  (setq awqat-asr-hanafi nil
        awqat-prayer-safety-offsets '(0.0 0.0 5.0 6.0 0.0 0.0)))

(defun awqat-set-preset-diyanet-standard  ()
  "Set the calculation method to the standard Diyanet İşleri Başkanlığı, Turkey."
  (awqat--preset-with-angles -18.0 -17.0)
  (setq awqat-prayer-safety-offsets '(0.0 -7.0 5.0 4.0 7.0 0.0)))

(defun awqat-set-preset-muslim-pro ()
  "Use the calculation method defined by the Muslim Pro app, non official."
  (awqat--preset-with-angles -18.13 -16.3))

(defun awqat-set-preset-muslim-world-league ()
  "Use the calculation method defined by the Muslim World League."
  (awqat--preset-with-angles -18.0 -17.0))

(defun awqat-set-preset-karachi-university-of-islamic-sciences ()
  "Use calculation method by Karachi University of Islamic Sciences (KUIS)."
  (awqat--preset-with-angles -18.0 -18.0))

(defun awqat-set-preset-umm-al-qura ()
  "Use the calculation method defined by Umm al-Qura University, Makkah."
  (setq awqat-fajr-angle -18.5
        awqat-isha-angle nil
        awqat-prayer-funs `(awqat--prayer-fajr
                            awqat--prayer-sunrise
                            awqat--prayer-dhuhr
                            awqat--prayer-asr
                            awqat--prayer-maghrib
                            ,(lambda (d)
                               (let ((maghrib-time (awqat--prayer-maghrib d))
                                     (ramadan-p (eq 9 (car (awqat--islamic-from-gregorian d)))))
                                 (list (+ (car maghrib-time) (if ramadan-p 2.0 1.5))
                                       (cadr maghrib-time)))))))

(defun awqat-set-preset-egyptian-general-authority-of-survey ()
  "Use the calculation method defined by the Egyptian General Authority of Survey."
  (awqat--preset-with-angles -19.5 -17.5))

(defun awqat-set-preset-kuwait ()
  "Use the calculation method used in Kuwait."
  (awqat--preset-with-angles -18.0 -17.5))

(defun awqat-set-preset-institute-of-geophysics-university-of-tehran ()
  "Use calculation method by the Institute of Geophysics, University of Tehran."
  (awqat--preset-with-angles -17.7 -14.0 -4.5))

(defun awqat-set-preset-jafari ()
  "Use calculation method used by Shia Ithna-Ashari, Leva Institute, Qum."
  (awqat--preset-with-angles -16.0 -14.0 -4.0))

(defun awqat-set-preset-jakim ()
  "Use calc method by Department of Islamic Development Malaysia (JAKIM)."
  (awqat--preset-with-angles -20.0 -18.0))

(defun awqat-set-preset-morocco ()
  "Use the calculation method used in Morocco."
  (awqat--preset-with-angles -19.0 -17.0))

(defun awqat-set-preset-taiwan ()
  "Use the calculation method used in Taiwan."
  (awqat--preset-with-angles -16.0 -19.0))

(defun awqat-set-preset-dubai ()
  "Use the calculation method used in Dubai, UAE."
  (awqat--preset-with-angles -18.2 -18.2))

(defun awqat-set-preset-gulf-region ()
  "Use the calculation method used in some countries in the Gulf region."
  (setq awqat-fajr-angle -19.5
        awqat-isha-angle nil
        awqat-isha-after-sunset-offset 1.5 ; 90min
        awqat-prayer-funs `(awqat--prayer-fajr
                            awqat--prayer-sunrise
                            awqat--prayer-dhuhr
                            awqat--prayer-asr
                            awqat--prayer-maghrib
                            awqat--prayer-isha-offset)))

(defun awqat-set-preset-qatar ()
  "Use the calculation method use in Qatar."
  (setq awqat-fajr-angle -18.0
        awqat-isha-angle nil
        awqat-isha-after-sunset-offset 1.5 ; 90min
        awqat-prayer-funs `(awqat--prayer-fajr
                            awqat--prayer-sunrise
                            awqat--prayer-dhuhr
                            awqat--prayer-asr
                            awqat--prayer-maghrib
                            awqat--prayer-isha-offset)))

(defun awqat-set-preset-spiritual-administration-of-muslims-russia ()
  "Use calculation method by Spiritual Administration of Muslims, Russia (SAMR)."
  (awqat--preset-with-angles -16.0 -15.0))

(defun awqat-set-preset-french-muslims ()
  "Use calculation method by the French Muslims.
Former: Union des Organisations Islamiques de France."
  (awqat--preset-with-angles -12.0 -12.0))

(defun awqat-set-preset-grande-mosquee-de-paris ()
  "Use calculation method similar to one used by Grande Mosquée de Paris, France.

Please note that these angles aren't official. In some references, it is
mentioned that the Grande Mosquée de Paris uses 18° for both Fajr and
Isha. However, the official prayer times table published by the mosque
doesn't match caclculations with these angles. The *guessed* -15 and -13
angles worked for some time, but doesn't predict the time well
currently."
  (awqat--preset-with-angles -15.0 -13.0))

(defun awqat-set-preset-isna ()
  "Use calculation method by Islamic Society of North America (ISNA)."
  (awqat--preset-with-angles -15.0 -15.0))

(defun awqat-set-preset-portugal ()
  "Use calculation method defined by Comunidade Islamica de Lisboa."
  (setq awqat-fajr-angle -18.0
        awqat-isha-angle nil
        awqat-isha-after-sunset-offset (/ 77.0 60.0) ; 77min
        awqat-maghrib-after-sunset-offset (/ 3.0 60.0) ; 3min
        awqat-prayer-funs `(awqat--prayer-fajr
                            awqat--prayer-sunrise
                            awqat--prayer-dhuhr
                            awqat--prayer-asr
                            awqat--prayer-maghrib-offset
                            awqat--prayer-isha-offset)))

(defun awqat-set-preset-jordan ()
  "Use calculation method defined by the Ministry of Awqaf, Islamic Affairs and Holy Places, Jordan."
  (setq awqat-fajr-angle -18.0
        awqat-isha-angle -18.0
        awqat-maghrib-after-sunset-offset (/ 5.0 60.0) ; 5min
        awqat-prayer-funs `(awqat--prayer-fajr
                            awqat--prayer-sunrise
                            awqat--prayer-dhuhr
                            awqat--prayer-asr
                            awqat--prayer-maghrib-offset
                            awqat--prayer-isha)))

(defun awqat-set-preset-high-latitudes (&optional method)
  "Use the calculation METHOD used in higher latitudes.

Supported methods:
  - `one-seventh-of-night'
  - `one-third-of-night'
  - `midnight'
  - `angle-based'

This needs to be set with some Fajr/Isha angles (use some angle method
preset or set them manually)."
  (setq awqat-high-latitudes-adjustment-method (or method 'angle-based)
        awqat-high-latitudes-adjustment-max-latitude 48.5
        awqat-prayer-funs '(awqat--prayer-fajr
                            awqat--prayer-sunrise
                            awqat--prayer-dhuhr
                            awqat--prayer-asr
                            awqat--prayer-maghrib
                            awqat--prayer-isha)))

(defun awqat-set-preset-moonsighting-committee-worldwide  ()
  "Use calculation method defined by the Moonsighting Committee Worldwide (MCW).
This is a latitude and season aware method."
  (setq awqat-fajr-angle -18.0
        awqat-isha-angle -18.0
        awqat-prayer-funs '(awqat--prayer-fajr-moonsighting
                            awqat--prayer-sunrise
                            awqat--prayer-dhuhr
                            awqat--prayer-asr
                            awqat--prayer-maghrib
                            awqat--prayer-isha-moonsighting)))

(defun awqat-set-preset-canada-13 ()
  "Use 13° calculation method used in some mosques in Canada."
  (awqat--preset-with-angles -13.0 -13.0))

(defalias 'awqat-set-preset-algeria 'awqat-set-preset-muslim-world-league
  "Use calculation method by Ministry of Religious Affairs and Wakfs, Algeria.")

(defalias 'awqat-set-preset-france-15 'awqat-set-preset-isna
  "Use 15° calculation method used in some mosques in France.")

(defalias 'awqat-set-preset-france-18 'awqat-set-preset-karachi-university-of-islamic-sciences
  "Use 18° calculation method used in some mosques in France.")

(defalias 'awqat-set-preset-indonesia 'awqat-set-preset-jakim
  "Use the calculation method defined by the Kementerian Agama Republik Indonesia.")

(defalias 'awqat-set-preset-singapore 'awqat-set-preset-jakim
  "Use the calculation method defined by the Majlis Ugama Islam Singapura.")

(defalias 'awqat-set-preset-tunisia 'awqat-set-preset-karachi-university-of-islamic-sciences
  "Use the calculation method used in Tunisia.")

(defalias 'awqat-set-preset-uae 'awqat-set-preset-gulf-region
  "Use the calculation method of UAE General Authority of Islamic Affairs And Endowments.")

(defalias 'awqat-set-preset-canada-15 'awqat-set-preset-isna
  "Use 15° calculation method used in some mosques in Canada.")

(defalias 'awqat-set-preset-canada-18 'awqat-set-preset-karachi-university-of-islamic-sciences
  "Use 18° calculation method used in some mosques in Canada.")

;;; Presets helper functions

(defun awqat--preset-with-angles (fajr isha &optional maghrib)
  "Use the standard angle method calculation with FAJR and ISHA angles.

Optionally, set the MAGHRIB angle."
  (setq awqat-fajr-angle fajr
        awqat-isha-angle isha
        awqat-maghrib-angle maghrib
        awqat-sunrise-sunset-angle -0.833 ; In case the user changed it
        awqat-prayer-funs '(awqat--prayer-fajr
                            awqat--prayer-sunrise
                            awqat--prayer-dhuhr
                            awqat--prayer-asr
                            awqat--prayer-maghrib
                            awqat--prayer-isha)))

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
  "Return tomorrow's date in the format (M D Y)."
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
  "Return the next time coming up."
  (let* ((now (awqat--now))
         (times (seq-map-indexed (lambda (time idx)
                                   (append time (list idx)))
                                 (awqat--times-for-day)))
         (after-times (seq-filter (lambda (time) (> (car time) now)) times)))
    (if after-times
        (car after-times)
      (append (car (awqat--times-for-day (awqat--tomorrow))) (list 0)))))

(with-suppressed-warnings ((lexical date))
  (defvar date)
  (defvar org-agenda-show-future-repeats))

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

(defun awqat-elevation-adjusted-angle (angle)
  "Correct ANGLE for elevation."
  (- angle (* 0.0347 (sqrt awqat-elevation))))

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

(defun awqat--moonsighting-get-offset (type)
  "Get the offset in hours for TYPE.
TYPE being a valid symbol for the `awqat-isha-moonsighting-method' variable.
Used by the Moonsighting Committee Worldwide method."
  (cl-destructuring-bind (a b c d)
      (awqat--moonsighting-constants type)
    (let* ((dyy (awqat--days-since-winter-solstice)))
      (/ (cond ((< dyy 91)   (+ a (* (/ (- b a) 91.0) dyy)))
               ((< dyy 137)  (+ b (* (/ (- c b) 46.0) (- dyy 91))))
               ((< dyy 183)  (+ c (* (/ (- d c) 46.0) (- dyy 137))))
               ((< dyy 229)  (+ d (* (/ (- c d) 46.0) (- dyy 183))))
               ((< dyy 275)  (+ c (* (/ (- b c) 46.0) (- dyy 229))))
               ((>= dyy 275) (+ b (* (/ (- a b) 91.0) (- dyy 275)))))
         60.0))))

(defun awqat--days-since-winter-solstice (&optional d)
  "Return the days count since the last winter solstice from D or today."
  (let* ((d (or d (awqat--today)))
         ;; Get the winter solstice (ID=3) for the year of `d'
         (winter-solstice (solar-equinoxes/solstices 3 (calendar-extract-year d)))
         ;; If it didn't happen yet for this year, get the previous year's one
         (winter-solstice (if (calendar-date-compare (list d) (list winter-solstice))
                              (solar-equinoxes/solstices 3 (1- (calendar-extract-year d)))
                            winter-solstice)))
    (- (calendar-absolute-from-gregorian d)
       (calendar-absolute-from-gregorian winter-solstice))))

(defun awqat--moonsighting-constants (type)
  "Return a list of (a b c d) constants for calculation TYPE, for a given LATITUDE."
  ;; CTE = alpha + beta / 55.0 * abs(latitude)
  (mapcar (lambda (ab) (+ (car ab) (* (/ (cadr ab) 55.0) (abs (calendar-latitude)))))
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
  (let ((time (car (awqat-sunrise-sunset-angle d awqat-fajr-angle))))
    (list (awqat--adjust-for-high-latitudes d (car time) awqat-fajr-angle -1) (cadr time))))

(defun awqat--prayer-fajr-diyanet (d)
  "Calculate the time of fajr for date D using third-portion if lat > 45."
  (let ((awqat-high-latitudes-adjustment-max-latitude 45.0)
        (awqat-high-latitudes-adjustment-method 'one-third-of-night))
    (awqat--prayer-fajr d)))

(defun awqat--adjust-for-high-latitudes (d time angle sign &optional method)
  "Get the high latitudes adjustment for TIME of date D.

Calculated with respect to ANGLE and with SIGN, with METHOD if
specified, otherwise, follow `awqat-high-latitudes-adjustment-method'."
  (if-let* (((or (eq awqat-high-latitudes-adjustment-max-latitude t)
                 (and (numberp awqat-high-latitudes-adjustment-max-latitude)
                      (> (abs (calendar-latitude)) (abs awqat-high-latitudes-adjustment-max-latitude)))))
            (fraction (pcase (or method awqat-high-latitudes-adjustment-method)
                        ('one-seventh-of-night (/ 1.0 7.0))
                        ('one-third-of-night (/ 1.0 3.0))
                        ('midnight (/ 1.0 2.0))
                        ('angle-based (/ (abs angle) 60.0))))
            (portion (* (awqat-night-duration d) fraction))
            (base (if (< sign 0) (awqat--sunrise d) (awqat--sunset d)))
            ((or (not time)
                 (> (* (- (if (< (* time sign) (* base sign))
                              (+ 24.0 time)
                            time)
                          base)
                       sign)
                    portion))))
      (mod (+ base (* portion sign)) 24.0)
    time))

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
  (when-let* ((sunrise (awqat--sunrise d)))
    (list (- sunrise awqat-fajr-before-sunrise-offset) (awqat--timezone d))))

(defun awqat--prayer-fajr-moonsighting (d)
  "Calculate the time of Fajr for a given date D.
The Moonsighting Committee Worldwide (MCW) method is a latitude
and season aware method.  It takes into account placed in higher
latitudes, up to 60°N/S."
  (cond ((< (abs (calendar-latitude)) 55.0)
         ;; From equator to 55°, the 18° depression angle calculations are compared with the values
         ;; given by the functions of latitude and seasons and most favorable values are used, which
         ;; means; for Fajr, the later of the two and for Isha the earlier of the two.
         (let* ((offset (awqat--moonsighting-get-offset 'subh-sadiq))
                (fajr-18 (car (awqat-sunrise-sunset-angle d -18.0)))
                (sunrise (car (awqat--prayer-sunrise d))))
           (list (max (car fajr-18) (- sunrise offset))
                 (awqat--timezone d))))
        ((and (<= 55.0 (abs (calendar-latitude))) (< (abs (calendar-latitude)) 60.0))
         (list (awqat--adjust-for-high-latitudes d nil nil -1 'one-seventh-of-night)
               (awqat--timezone d)))
        (t (warn "Latitudes beyond 60°N/S, hardship prevails and beyond 65°,
the sun does not set/rise for a number of days every year."))))

;; Sunrise

(defun awqat--prayer-sunrise (d)
  "Calculate the time of the sunrise on a given date D."
  (list (awqat--sunrise d) (awqat--timezone d)))

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
  "Calculate the time of Maghrib on date D."
  (let* ((angle (awqat-elevation-adjusted-angle (or awqat-maghrib-angle awqat-sunrise-sunset-angle)))
         (time (cadr (awqat-sunrise-sunset-angle d angle))))
    (list
     (if awqat-high-latitudes-adjust-maghrib
         (awqat--adjust-for-high-latitudes d (car time) angle 1)
       (car time))
     (cadr time))))

(defun awqat--prayer-maghrib-offset (d &optional offset)
  "Calculate the time of Maghrib on date D, with optional OFFSET."
  (list (+ (or offset awqat-maghrib-after-sunset-offset) (awqat--sunset d))
        (awqat--timezone d)))

;; Isha

(defun awqat--prayer-isha (d)
  "Calculate the time of Isha for date D using standard angle method."
  (let* ((time (cadr (awqat-sunrise-sunset-angle d awqat-isha-angle))))
    (list (awqat--adjust-for-high-latitudes d (car time) awqat-isha-angle 1) (cadr time))))

(defun awqat--prayer-isha-diyanet (d)
  "Calculate the time of Isha for date D using third-portion if lat > 45."
  (let ((awqat-high-latitudes-adjustment-max-latitude 45.0)
        (awqat-high-latitudes-adjustment-method 'one-third-of-night))
    (awqat--prayer-isha d)))

(defun awqat--prayer-isha-angle (d)
  "Calculate the time of Isha for date D using angle method if necessary."
  (if (awqat--use-angle-method-p d)
      (let ((offset (awqat--angle-approx-offset d awqat-isha-angle))
            (timezone (awqat--timezone d))
            (sunset (awqat--sunrise d)))
        (list (+ sunset offset) timezone))
    (awqat--prayer-isha d)))

(defun awqat--prayer-isha-offset (d &optional offset)
  "Calculate the time of Isha based on fixed time for given date D.

When OFFSET is non-nil, use it instead of `awqat-isha-after-sunset-offset'."
  (when-let* ((sunset (awqat--sunset d)))
    (list (+ sunset (or offset awqat-isha-after-sunset-offset)) (awqat--timezone d))))

(defun awqat--prayer-isha-moonsighting (d)
  "Calculate the time of Isha for a given date D.
The Moonsighting Committee Worldwide (MCW) method is a latitude
and season aware method.  It takes into account placed in higher
latitudes, up to 60°N/S."
  (cond ((< (abs (calendar-latitude)) 55.0)
         ;; From equator to 55°, the 18° depression angle calculations are compared with the values
         ;; given by the functions of latitude and seasons and most favorable values are used, which
         ;; means; for Fajr, the later of the two and for Isha the earlier of the two.
         (let* ((offset (awqat--moonsighting-get-offset awqat-isha-moonsighting-method))
                (isha-18 (cadr (awqat-sunrise-sunset-angle d -18.0))))
           (list (awqat--isha-time-min
                  d (car isha-18) (+ (car (awqat--prayer-maghrib d)) offset))
                 (awqat--timezone d))))
        ((and (<= 55.0 (abs (calendar-latitude))) (< (abs (calendar-latitude)) 60.0))
         (list (awqat--adjust-for-high-latitudes d nil nil 1 'one-seventh-of-night)
               (awqat--timezone d)))
        (t (warn "Latitudes beyond 60°N/S, hardship prevails and beyond 65°,
the sun does not set/rise for a number of days every year."))))

;;; Time Calculations --------------------------------------------------------------------

(defun awqat--angle-approx-offset (d angle)
  "Calculate the time of isha for date D at with ANGLE using andle method.

This method calculates the time based on a portion of angle/60 of the night."
  (let ((night-duration (awqat-night-duration d)))
    (/ (* (abs angle) night-duration) 60)))

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
  "Determine if on date D, the angle method for Isha/Fajr should be used."
  (let ((isha (caadr (awqat-sunrise-sunset-angle d awqat-isha-angle)))
        (fajr (caar (awqat-sunrise-sunset-angle d awqat-fajr-angle))))
    (or (not isha)
        (not fajr)
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
  "Works like `min', used to compare Isha TIME1 and TIME2 for date D.
This takes into account the midnight comparaison, so 23.0 is before 00.2.
Do not use for times other than Isha."
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
    res))

(defun awqat-daylight-duration (d)
  "Return the duration in hours from sunrise to sunset for a given date D."
  (- 24.0 (awqat-night-duration d)))

(defun awqat-night-duration (d)
  "Return the duration in hours from sunset to sunrise for a given date D."
  (if-let* ((rise-set (awqat-sunrise-sunset d))
            (sunrise (caar rise-set))
            (sunset (caadr rise-set)))
      (+ 24.0 (- sunrise sunset))
    0.0))

(defun awqat-solar-noon (d)
  "Calculate the time of the noon on date D."
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

(defun awqat--format (fmt &rest label-val-cons)
  "Use format FMT to format the LABEL-VAL-CONS.
Calling:
  \\=(awqat--format \"${one}, ${two}, ${one}\" \\='(\"one\" . 1) \\='(\"two\" . 2))
Produces: \"1, 2, 1\"."
  (let ((out fmt))
    (dolist (label-var label-val-cons)
      (setq out (string-replace (format "${%s}" (car label-var)) (format "%s" (cdr label-var)) out)))
    out))

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
             (message (awqat--format awqat-mode-line-format (cons "prayer" name) (cons "hours" h) (cons "minutes" m)))
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

(defcustom awqat-audio-player (seq-find #'executable-find '("ffplay" "paplay" "afplay" "aplay"))
  "Music player used to play sounds.
Possible values are \"ffplay\", \"paplay\", \"aplay\", or \"afplay\"."
  :group 'awqat
  :type '(choice (const "ffplay")
                 (const "paplay")
                 (const "afplay")
                 (const "aplay")))

(defun awqat--play-sound (sound-file)
  "Create and return a process to play SOUND-FILE.
The program to use is specified in the variable `awqat-audio-player'."
  (when awqat-audio-player
    (let ((cmd-args
           (if (equal "ffplay" (file-name-base awqat-audio-player))
               (list awqat-audio-player "-nodisp" "-autoexit" sound-file)
             (list awqat-audio-player sound-file))))
      (apply #'start-process (append '("awqat-sound-process" "*awqat-sound-process*") cmd-args)))))

(defcustom awqat-play-adhan-for-times '(t nil t t t t)
  "List, corresponding to elements of `awqat-prayer-funs'.
Used to determine if adhan should be played.  For example, for the
default value of `awqat-prayer-funs', setting this variable to
the value (t nil t t t t) would result it all sounds playing
except ishak.  A non-nil value indicates that the adhan should
play.  If the value is a string, it is interpreted as a specific
file to play for the specified time."
  :group 'awqat
  :type '(list boolean))

(defcustom awqat-adhan-file nil
  "Path to the sound file to play when the prayer time is reached."
  :type '(file :must-match t)
  :group 'awqat)

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

(defun awqat-stop-adhan ()
  "Stop the currently playing sound."
  (interactive)
  (when awqat--adhan-process
    (delete-process awqat--adhan-process)
    (setq awqat--adhan-process nil)
    (message "Adhan stopped.")))

;;;###autoload
(define-minor-mode awqat-adhan-mode
  "Toggle the playing of the adhan for each time."
  :global t
  (and awqat--next-adhan-timer (cancel-timer awqat--next-adhan-timer))
  (awqat-stop-adhan)
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
