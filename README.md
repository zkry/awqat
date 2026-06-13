# Awqat - Islamic prayer times for Emacs

Awqat is an Emacs package that calculates daily Islamic prayer times and
provides optional mode-line display, Adhan playback, and desktop notifications.


## Installation

Awqat is now available in MELPA under the name
[awqat](https://melpa.org/#/awqat). You can install it using your preferred
method (Elpaca, straight, M-x list-packages, etc.)


### Installation from source

You can install Awqat from source using the following use-package call (note
this requires Emacs version 30+):

```lisp
(use-package awqat
  :vc (:url "https://github.com/zkry/awqat"
       :rev :newest))
```


### Installation for Spacemacs
If you are using [Spacemacs](https://github.com/syl20bnr/spacemacs) you can
easily install this package by putting the following in the
`dotspacemacs-additional-packages`:

```lisp
dotspacemacs-additional-packages
'((awqat :location (recipe
                    :fetcher github
                    :repo "zkry/awqat")))
```

And adding `(require 'awqat)` in `dotspacemacs/user-config`.


### Installation for Doom Emacs

If you are using [Doom Emacs](https://github.com/doomemacs/doomemacs), you can
declare a custom package in your `packages.el` file:

```lisp
(package! awqat
  :recipe (:host github
           :repo "zkry/awqat"))
```

And then, add a suitable configuration based on your location.

```lisp
(use-package! awqat
  :commands (awqat-display-prayer-time-mode
             awqat-times-for-day)
  :config
  (setq calendar-latitude 44.2
        calendar-longitude 1.3
        awqat-mode-line-format " 🕌 ${prayer} (${hours}h${minutes}m) "))
  (awqat-set-preset-muslim-world-league))
```


## Setup

You should have your calendar's latitude and longitude variables set up.
Evaluate the variables `calendar-latitude` and `calendar-longitude` to see if
these are set properly.

The prayer times can be configured in the following ways:

### Fajr

- If using an angle-based method, you can set the angle to calculate Fajr with
  the `awqat-fajr-angle` variable (used in conjunction with the
  `awqat-use-angle-based-method` function or any angle-based method).
- If using a fixed offset-based method, you can set the hours before sunrise via
  the `awqat-fajr-before-sunrise-offset` variable (used in conjunction with the
  `awqat-use-time-offset-method` function or any fixed offset-based method).


### Sunrise

- The default astronomical angle `awqat-sunrise-sunset-angle` is a **constant**
  set to -0.833. It represents the sunrise/sunset zenith angle offset below the
  horizon. A zero value corresponds to the sun being at zenith=90°, which means
  that the sun circle is still visible. The apparent radius of the sun at the
  horizon is 16 arcminutes, and the average refraction is known to be 34
  arcminutes, which gives an offset of 50 arcminutes, hence the -0.833° value.
  **You shouldn't change this variable unless you are an astrophysicist!**


### Dhuhr

- Dhuhr time corresponds to noon. Dhuhr does not have a specific configuration
  other than safety offsets (see below).


### Asr

- You can set the flag `awqat-asr-hanafi` to `t` to follow the Hanafi opinion on
  Asr time determination. Or set it to `nil` for the consensus (al-Jomhur)
  opinion (of Hanbali, Shafii, and Maliki). Defaults to `nil`.


### Maghrib

- Like sunrise, the angle `awqat-sunrise-sunset-angle` is used to calculate the
  sunset (Maghrib) time.
- You can override Maghrib angle via `awqat-maghrib-angle`. This is used by some
  methods (mainly, Shia ones). For other methods, it is highly discouraged to
  change it, unless you have a very strong and justified argument to do!
- Sunset offset mode (used by some presets), in this mode the Maghrib time will
  be determined by adding a fixed offset the the sunset time via
  `awqat-maghrib-after-sunset-offset`.
- By default, Maghrib time is calculated at elevation of 0m, however, you can
  set the elevation above see level of your city in `awqat-elevation` (in
  meters). This can provide a correction if the observed sunset time is late
  than the one reported by the default calculation at 0m elevation.


### Isha

- Like for Fajr you can set the angle for calculation with the
  `awqat-isha-angle` variable (used in conjunction with the
  `awqat-use-angle-based-method` function or any angle-based method)
- If using the offset-based method, you can set the hours after sunset via the
  `awqat-isha-after-sunset` variable (used in conjunction with the
  `awqat-use-time-offset-method` function or any fixed offset-based method).
- In addition to the specified angle-based method used, people living in higher
  latitudes (beyond 48.5N, 48.5S) can configure some extra parameters:
  - `awqat-high-latitudes-adjustment-max-latitude` to set the latitude beyond
    which, the place is considered high latitude which defaults to 48.5
    following the definition of Moonsighting Worldwide Committee. So, unless
    highly justified, please stick the the default value.
  - `awqat-high-latitudes-adjustment-method` can be used to specify the adjustment
    method, it can be one of `angle-based` (default), `one-seventh-of-night`,
    `one-third-of-night`, or `midnight`.
  - By default, the high latitudes adjustment is only applied to Fajr and Isha, if
    you want to apply it also to Maghrib, you can set
    `awqat-high-latitudes-adjust-maghrib` to non-nil.
- For the Moonsighting Committee Worldwide method, an additional parameter can
  be set for Isha. The `awqat-isha-moonsighting-method` accepts a symbol which
  can be `'shafaq-ahmar`, `'shafaq-abyad`, or `'shafaq` (which is a combination
  of _Shafaq Ahmar_ and _Shafaq Abyad_ for high latitudes).


### Safety offsets

You can add a safety offset to all times via the `awqat-prayer-safety-offsets`
variable. For example, to have sunrise be one minute sooner, Dhuhr two minutes
later, and Maghrib one minute later you can add `(setq
awqat-prayer-safety-offsets '(0.0 -1.0 2.0 0.0 1.0 0.0))` to your configuration.

### Presets

There are presets with the angles for various organizations. You can call these
functions to configure corresponding calculation parameters. The following
presets are defined:

- `awqat-set-preset-diyanet`: Set the calculation method defined by Diyanet İşleri Başkanlığı, Turkey.
- `awqat-set-preset-diyanet-standard`: Set the calculation method to the standard Diyanet İşleri Başkanlığı, Turkey.
- `awqat-set-preset-muslim-pro`: Use the calculation method defined by the Muslim Pro app, non official.
- `awqat-set-preset-muslim-world-league`: Use the calculation method defined by the Muslim World League.
- `awqat-set-preset-karachi-university-of-islamic-sciences`: Use calculation method by Karachi University of Islamic Sciences (KUIS).
- `awqat-set-preset-umm-al-qura`: Use the calculation method defined by Umm al-Qura University, Makkah.
- `awqat-set-preset-egyptian-general-authority-of-survey`: Use the calculation method defined by the Egyptian General Authority of Survey.
- `awqat-set-preset-kuwait`: Use the calculation method used in Kuwait.
- `awqat-set-preset-institute-of-geophysics-university-of-tehran`: Use calculation method by the Institute of Geophysics, University of Tehran.
- `awqat-set-preset-jafari`: Use calculation method used by Shia Ithna-Ashari, Leva Institute, Qum.
- `awqat-set-preset-jakim`: Use calculation method by Department of Islamic Development Malaysia (JAKIM).
- `awqat-set-preset-morocco`: Use the calculation method used in Morocco.
- `awqat-set-preset-taiwan`: Use the calculation method used in Taiwan.
- `awqat-set-preset-dubai`: Use the calculation method used in Dubai, UAE.
- `awqat-set-preset-gulf-region`: Use the calculation method used in some countries in the Gulf region.
- `awqat-set-preset-qatar`: Use the calculation method use in Qatar.
- `awqat-set-preset-spiritual-administration-of-muslims-russia`: Use calculation method by Spiritual Administration of Muslims, Russia (SAMR).
- `awqat-set-preset-french-muslims`: Use calculation method by the French Muslims.
- `awqat-set-preset-france-15`: Use 15° calculation method used in some mosques in France.
- `awqat-set-preset-france-18`: Use 18° calculation method used in some mosques in France.
- `awqat-set-preset-isna`: Use calculation method by Islamic Society of North America (ISNA).
- `awqat-set-preset-portugal`: Use calculation method defined by Comunidade Islamica de Lisboa.
- `awqat-set-preset-jordan`: Use calculation method defined by the Ministry of Awqaf, Islamic Affairs and Holy Places, Jordan.
- `awqat-set-preset-high-latitudes`: Use the calculation METHOD used in higher latitudes.
- `awqat-set-preset-moonsighting-committee-worldwide`: Use calculation method defined by the Moonsighting Committee Worldwide (MCW).
- `awqat-set-preset-canada-13`: Use 13° calculation method used in some mosques in Canada.
- `awqat-set-preset-algeria`: Use calculation method by Ministry of Religious Affairs and Wakfs, Algeria.
- `awqat-set-preset-indonesia`: Use the calculation method defined by the Kementerian Agama Republik Indonesia.
- `awqat-set-preset-singapore`: Use the calculation method defined by the Majlis Ugama Islam Singapura.
- `awqat-set-preset-tunisia`: Use the calculation method used in Tunisia.
- `awqat-set-preset-uae`: Use the calculation method of UAE General Authority of Islamic Affairs And Endowments.
- `awqat-set-preset-canada-15`: Use 15° calculation method used in some mosques in Canada.
- `awqat-set-preset-canada-18`: Use 18° calculation method used in some mosques in Canada.


## Configuration example

The following is an example configuration:

```lisp
(require 'awqat)
(setq calendar-latitude 52.439
      calendar-longitude 13.436
      awqat-fajr-angle -18.0
      awqat-isha-angle -16.0)
```

Here is another example that makes use of the Muslim World League preset:

```lisp
(require 'awqat)
(setq calendar-latitude 52.439
      calendar-longitude 13.436
      awqat-asr-hanafi nil)
(awqat-set-preset-muslim-world-league)
```

Here is another example that play athan sound on the scheduled time and stop with keyboard shortcut
```lisp
(use-package awqat
  :quelpa (awqat :fetcher github :repo "zkey/awqat")
  :bind (("C-c r p" . awqat-times-for-day)
         ("C-c r s" . awqat-stop-adhan))
  :init
  (awqat-display-prayer-time-mode 1)
  (awqat-adhan-mode 1)
  :config
  (setq calendar-latitude 40.9
        calendar-longitude -74.3
        ;; Executable ffplay required to play mp3 file
        awqat-adhan-file (expand-file-name "~/Drive/adhan.mp3")
        awqat-mode-line-format " 🕌 ${prayer} (${hours}h${minutes}m) "
        awqat-prayer-safety-offsets '(-6.0 -9.0 5.0 4.0 5.0 -1.0)
        org-agenda-include-diary t)
  (awqat-set-preset-muslim-world-league))
```

Enable desktop notifications: 

```lisp
(awqat-notification-mode 1)
```


## Viewing the times

By calling `M-x awqat-times-for-day` you can see the six times for the day,
alongside the remaining time for the next prayer.


### Viewing times with Diary/Org-Agenda

The times can be added to the diary view or org-agenda view using the diary
functions. In your `diary-file`, add the following lines:

```
%%(awqat-diary-fajr)
%%(awqat-diary-sunrise)
%%(awqat-diary-dhuhr)
%%(awqat-diary-asr)
%%(awqat-diary-maghrib)
%%(awqat-diary-isha)
```

To use times with Org-mode, set `org-agenda-include-diary` to `t`:

```lisp
(setq org-agenda-include-diary t)
```

Then in an Org file which is part of `org-agenda-files`, add the following:

```org
* Prayers
  :PROPERTIES:
  :CATEGORY: prayers
  :END:
%%(awqat-diary-fajr)
%%(awqat-diary-sunrise)
%%(awqat-diary-dhuhr)
%%(awqat-diary-asr)
%%(awqat-diary-maghrib)
%%(awqat-diary-isha)
```

The above snippets are, of course, examples. Feel free to modify your liking.

## `awqat-display-prayer-time-mode`
By running the command `awqat-display-prayer-time-mode` you can view the
upcoming prayer time in the mode-line which is updated in real-time.

## `awqat-adhan-mode`
By running the command `awqat-adhan-mode`, an Adhan audio file will be played at
each prayer time. For this to work, you should have installed `ffplay` (part of
[FFMPG](https://ffmpeg.org/), used to play mp3 files) or if your Adhan sound
file is a wav file, you can use `aplay` or `afplay`. Ensure the variable
`awqat-audio-player` is set to the correct audio player.

Next ensure that `awqat-adhan-file` is set to be the Adhan file to play.

Finally, you can configure `awqat-play-adhan-for-times` to be indicate the
prayer times you would like to hear the Adhan for. An example value is
`'("~/Downloads/fajr.mp3" nil t t t t)` to configure a special file to be played
for Fajr, ignore sunrise, and use the default Adhan file to be played for the
remaining times.

To stop a playing Adhan, you can run the command `awqat-stop-adhan`.


## Notes on the calculation methods

Please be warned that this package may contain bugs, the times calculated by
Awqat may or may not reflect the times of the particular organization that you
follow. We encourage you to check the Awqat times against the times of your
organization or your local mosque. Please feel free to open an issue if you
observe an anomaly in our calculations.


### High latitudes

The calculations for Isha and Fajr for high latitudes are implemented through
_**midnight**_, _**one-seventh of the night**_, _**one-seventh of the night**_,
and the _**twilight angle-based**_ methods[^praytimes].

If you live in a high latitude place (beyond 48.5°N and 48.5°S), you should
check your organization or mosque to confirm the accepted method. The current
implementation checks automatically if you are in a high latitude and applies
adjustments based on the specified method (see above).

Note that, the _midnight_ method (used by some organizations in very high
latitude places) tries to resolve the problem of estimating the Fajr time by
choosing the Fajr time to be at _astronomical midnight_. The time between sunset
and the astronomical midnight is considered to be the Night. The _midnight_
method does not clearly define the Isha time, hence you need to check with your
local organization or mosque to see which time is considered Isha's time. Note
that some jurisprudence (al-Fiqh الفقه) opinions adopt grouping Maghrib and Isha
prayers (al-Jam'a الجمع) when astronomical signs are not visible (as in very
high latitudes during some period of time per year).


### Moonsighting Committee Worldwide method

Awqat implements the Moonsighting Committee Worldwide (MCW)
method[^moonsighting]. This method is location and season aware.

For placed between the equator and 55°, this method defines correction
**functions based on the latitude and the season** to calculate variable offsets
from the sunset and sunrise. These offsets are then added or subtracted from the
sunset/sunrise to get the estimated prayer time. The time estimated using these
**functions** is then compared to the angle-based time calculated using the 18.0
angle, the most favorable is used (for Fajr, the latter of the two; for Isha,
the earlier of the two).

For places between 55° and 60°, the MCW uses the *one-seventh of night* method,
and beyond 60°, the method does not define an estimation method. If your city is
beyond the 60° limit, you should check with your local organizations.


[^praytimes]: [PrayTimes.org](https://praytimes.org/docs/calculation)
[^moonsighting]: [Syed Khalid Shaukat, _Fajr and Isha_, September 2015](https://1x.ax/islamic-network/libraries/prayer-times-moonsighting/~raw/master/booklet-fajr-isha.pdf?disposition=ATTACHMENT)
