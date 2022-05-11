# Awqat

Awqat is an Emacs package to calculate the five daily Islamic prayer times. It
includes also a mode line mode to show the remaining time to next prayer.

## Installation

This package is currently not located on MELPA. To use it you should include the
source file `awqat.el` in your source path.

### Installation for Spacemacs

If you are using [Spacemacs](https://github.com/syl20bnr/spacemacs) you can easily install this package by putting the
following in the `dotspacemacs-additional-packages`:

```lisp
dotspacemacs-additional-packages '((awqat :location (recipe
                                                         :fetcher github
                                                         :repo "zkry/awqat")))
```

And in `dotspacemacs/user-config` adding `(require 'awqat)`.

### Installation for Doom Emacs
If you are using [Doom Emacs](https://github.com/doomemacs/doomemacs), you can declare a custom package in your
`packages.el` file:

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

;; If you want to enable 'awqat-display-prayer-time-mode' at startup
(awqat-display-prayer-time-mode)
```

## Setup

You should have your calendar's latitude and longitude variables set up.
Evaluate the variables `calendar-latitude` and `calendar-longitude` to see if
these are set properly.

The prayer times can be configured in the following ways:

### Fajr
- If using angle-based method, you can set the angle to calculate Fajr with the
  `awqat-fajr-angle` variable. (used in conjunction with the
  `awqat-use-angle-based-method` function)
- If using a fixed offset-based method, you can set the hours before sunrise via
  the `awqat-fajr-before-offset` variable. (used in conjunction with the
  `awqat-use-time-offset-method` function)
- For people living in higher latitudes (beyond 48.5N, 48.5S), you can use:
  - The `awqat-set-preset-midnight` preset which uses the function
    `awqat--prayer-fajr-midnight` to calculate a prayer time for Fajr based on
    the midnight method.
  - The `awqat-set-preset-one-seventh-of-night` preset which uses the function
    `awqat--prayer-fajr-one-seventh-of-night` to calculate a prayer time for
    Fajr based on the one-seventh of night method. 

### Sunrise
- The only customizable variable for sunrise is `awqat-sunrise-sunset-angle`
  (Defaults to -0.833), which represents the sunrise/sunset zenith angle offset
  below horizon. A zero value corresponds to the sun being at zenith=90°, which
  means that the sun circle is still visible. The apparent radius of the sun at
  the horizon is 16 arcminutes, and the average refraction is known to be 34
  arcminutes, which gives an offset of 50 arcminutes, hence the -0.833° value.
  **You shouldn't change this variable unless you are an astrophysicist!**

### Dhuhr
Dhuhr time corresponds to the noon. It is the base for calculating other times.
Dhuhr do not have specific configuration. However, you can set safety offsets
for Dhuhr (see below).

### Asr
- You can set the flag `awqat-asr-hanafi` to `t` to follow the Hanafi opinion on
  Asr time determination. Or set it to `nil` for the consensus (al-Jomhur)
  opinion (of Hanbali, Shafii, and Maliki). Defaults to `nil`.

### Maghrib
- Like sunrise, only `awqat-sunrise-sunset-angle` can be set. However, it is
  highly discouraged to change it, unless you have a very strong an justified
  argument to do!

### Isha
- Like for Fajr you can set the angle for calculation with the
  `awqat-isha-angle` variable. (used in conjunction with the
  `awqat-use-angle-based-method` function)
- If using the offset-based method, you can set the hours after sunset via the
  `awqat-isha-after-sunset` variable. (used in conjunction with the
  `awqat-use-time-offset-method` function)
- For people living in higher latitudes (beyond 48.5N, 48.5S), you can use:
  - The `awqat-set-preset-midnight` preset which uses the function
    `awqat--prayer-isha-midnight` to calculate a prayer time for Isha based on
    the midnight method.
  - The `awqat-set-preset-one-seventh-of-night` preset which uses the function
    `awqat--prayer-isha-one-seventh-of-night` to calculate a prayer time for
    Isha based on the one-seventh of night method.
- For the Moonsighting Committee Worldwide method, an additional parameter can
  be set for Isha. The `awqat-isha-moonsighting-method` accepts a symbol which
  can be `'shafaq-ahmar`, `'shafaq-abyad`, or `'shafaq` (which is a combination
  of Shafaq Ahmar and Abyad for high latitudes).

### Safety offsets
You can add a safety offset to all times via the `awqat-prayer-safety-offsets`
variable. For example, to have sunrise be one minute sooner, Dhuhr two minutes
later, and Maghrib one minute later you can add `(setq
awqat-prayer-safety-offsets '(0.0 -1.0 2.0 0.0 1.0 0.0))`.

### Presets
There are presets with the angles for various organizations. You can call these
functions to configure corresponding calculation parameters. The following
presets are implemented:
 
- `awqat-set-preset-muslim-pro`
- `awqat-set-preset-muslim-world-league`
- `awqat-set-preset-karachi-university-of-islamic-sciences`
- `awqat-set-preset-umm-al-qura`
- `awqat-set-preset-jakim`
- `awqat-set-preset-spiritual-administration-of-musilms-russia`
- `awqat-set-preset-french-muslims`
- `awqat-set-preset-grande-mosquee-de-paris`
- `awqat-set-preset-isna`
- `awqat-set-preset-egyptian-general-authority-of-survey`
- `awqat-set-preset-kuwait`
- `awqat-set-preset-algeria`
- `awqat-set-preset-morocco`
- `awqat-set-preset-taiwan`
- `awqat-set-preset-singapore`
- `awqat-set-preset-diyanet-turkey`
- `awqat-set-preset-uae`
- `awqat-set-preset-midnight`
- `awqat-set-preset-one-seventh-of-night`
- `awqat-set-preset-moonsighting-committee-worldwide`

## Configuration example
The following is an example configuration:

```lisp
(require 'awqat)
(setq calendar-latitude 52.439
      calendar-longitude 13.436)
(setq awqat-asr-hanafi nil)
(setq awqat-fajr-angle -18.0)
(setq awqat-isha-angle -16.0)
```

Here another example which make use of the Muslim World League preset:

```lisp
(require 'awqat)
(setq calendar-latitude 52.439
      calendar-longitude 13.436)
(setq awqat-asr-hanafi nil)
(awqat-set-preset-muslim-world-league)
```

## Viewing the times

By calling `M-x awqat-times-for-day` you can see the six times for the day,
alongside with the remaining time for the next prayer.

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

The above snippets are, of course, examples.  Feel free to modify to your liking.

## awqat-display-prayer-time-mode

By running the command `awqat-display-prayer-time-mode` you can view
the upcoming prayer time in the modeline which is updated in real time.

## Notes on the calculation methods

Please be warned that this package may contain bugs, the times calculated by
Awqat may or may not reflect the times of the particular organization that you
follow. We encourage you to check the Awqat times against times of your
organization or your local mosque. Please feel free to open an issue if you
observe an anomaly in our calculations.

### Moonsighting Committee Worldwide method
Awqat implements the Moonsighting Committee Worldwide (MCW) method[^moonsighting].
This method is location and season aware. 

For placed between equator and 55°, this method defines a set **functions of
latitude and season** to calculate variable offsets from the sunset and sunrise.
These offsets are then added or subtracted from the sunset/sunrise to get the
estimated prayer time. The time estimated using these **functions** is then
compared to the angle-based time calculated using the 18.0 angle, the most
favorable is used (for Fajr, the later of the two and for Isha the earlier of
the two).

For places between 55° and 60°, the MCW uses the *one-seventh of night* method.

### High latitudes
The calculations for Isha and Fajr for high latitudes are implemented through
the *midnight* and *one-seventh of the night* methods [^prayertimes]. The 
*angle-based* methods for calculating times at higher latitudes is not
implemented yet.

If you leave in a high altitude place (beyond 48.5°N and 48.5°S), you should
check your organization or mosque to confirm the accepted method. Also, these
special methods should only be used when the astronomical signs of twilight
(Fajr and Isha) are not visible, this occurs only during a specific period of
the year. Currently the method cannot determine whether the signs are visible or
not. So please make sure you are using the alternative method only when the
signs are not visible.

Note that, the *midnight* method tries to resolve the problem of estimating the
Fajr time by choosing the Fajr time to be at *astronomical midnight*. The time
between sunset and the astronomical midnight is considered to be the Night.
The *midnight* method do not clearly define the Isha time, hence you need to
check with your local organization or mosque to see which time is considered
Isha's time. Note that some jurisprudence (al-Fiqh الفقه) opinions adopt
grouping Maghrib and Isha prayers (al-Jam'a الجمع) when astronomical signs are
not visible (as in high latitudes during some period of time per year).

[^moonsighting]: [Syed Khalid Shaukat, _Fajr and Isha_, September 2015](https://raw.githubusercontent.com/islamic-network/prayer-times-moonsighting/master/booklet-fajr-isha.pdf)
[^prayertimes]: [Prayertimes.org](http://prayertimes.org/calculation)
