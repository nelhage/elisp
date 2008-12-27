;; dvc-annotate.el
;; (Copyed from vc.el --- drive a version-control system from within Emacs)
;;

;; Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 2000,
;;   2001, 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

;; Author:     FSF (see below for full credits)
;; Maintainer: Andre Spiegel <spiegel@gnu.org>
;; Keywords: tools

;; $Id$

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Credits:

;; VC was initially designed and implemented by Eric S. Raymond
;; <esr@snark.thyrsus.com>.  Over the years, many people have
;; contributed substantial amounts of work to VC.  These include:
;;   Per Cederqvist <ceder@lysator.liu.se>
;;   Paul Eggert <eggert@twinsun.com>
;;   Sebastian Kremer <sk@thp.uni-koeln.de>
;;   Martin Lorentzson <martinl@gnu.org>
;;   Dave Love <fx@gnu.org>
;;   Stefan Monnier <monnier@cs.yale.edu>
;;   J.D. Smith <jdsmith@alum.mit.edu>
;;   Andre Spiegel <spiegel@gnu.org>
;;   Richard Stallman <rms@gnu.org>
;;   Thien-Thi Nguyen <ttn@gnu.org>


;; Changes made to vc.el by Takuzo O'hara, <takuzo.ohara@gmail.com>
;;
;;     -. Removed parts not required in annotation.
;;     -. Modified names with vc.. -> dvc.. to not to conflict with
;;        vc.el.
;;     -. Changed (vc-call-backend ...) to use static values defined
;;        in below.

(defalias 'dvc-annotate-current-time 'dvc-default-annotate-current-time)

;;
;; -------------------------------------------------
;;

(defmacro dvc-annotate-8color-tty-p ()
  "Determine whether we are on a tty that uses 8 or less colors."
  (cond ((fboundp 'tty-display-color-p)
         `(and (tty-display-color-p)
               (<= (display-color-cells) 8)))
        ((and (fboundp 'display-color-p) (fboundp 'device-or-frame-type))
         ;; XEmacs 21
         `(and (display-color-p)
               (eq (device-or-frame-type (frame-device)) 'tty)))))

(defmacro dvc-annotate-tty-color-alist ()
  "Return a list of colors, each element of which is a list."
  (cond ((fboundp 'tty-color-alist)
         `(tty-color-alist))
        ((fboundp 'tty-color-list)
         `(mapcar #'list (tty-color-list)))))

;; Annotate customization
(defcustom dvc-annotate-color-map
  (if (dvc-annotate-8color-tty-p)
      ;; A custom sorted TTY colormap
      (let* ((colors
              (sort
               (delq nil
                     (mapcar (lambda (x)
                               (if (not (or
                                         (string-equal (car x) "white")
                                         (string-equal (car x) "black") ))
                                   (car x)))
                             (dvc-annotate-tty-color-alist)))
               (lambda (a b)
                 (cond
                  ((or (string-equal a "red") (string-equal b "blue")) t)
                  ((or (string-equal b "red") (string-equal a "blue")) nil)
                  ((string-equal a "yellow") t)
                  ((string-equal b "yellow") nil)
                  ((string-equal a "cyan") t)
                  ((string-equal b "cyan") nil)
                  ((string-equal a "green") t)
                  ((string-equal b "green") nil)
                  ((string-equal a "magenta") t)
                  ((string-equal b "magenta") nil)
                  (t (string< a b))))))
             (date 20.)
             (delta (/ (- 360. date) (1- (length colors)))))
        (mapcar (lambda (x)
                  (prog1
                      (cons date x)
                    (setq date (+ date delta)))) colors))
    ;; Normal colormap: hue stepped from 0-240deg, value=1., saturation=0.75
    '(( 20. . "#FF3F3F")
      ( 40. . "#FF6C3F")
      ( 60. . "#FF993F")
      ( 80. . "#FFC63F")
      (100. . "#FFF33F")
      (120. . "#DDFF3F")
      (140. . "#B0FF3F")
      (160. . "#83FF3F")
      (180. . "#56FF3F")
      (200. . "#3FFF56")
      (220. . "#3FFF83")
      (240. . "#3FFFB0")
      (260. . "#3FFFDD")
      (280. . "#3FF3FF")
      (300. . "#3FC6FF")
      (320. . "#3F99FF")
      (340. . "#3F6CFF")
      (360. . "#3F3FFF")))
  "Association list of age versus color, for \\[dvc-annotate].
Ages are given in units of fractional days.  Default is eighteen
steps using a twenty day increment, from red to blue.  For TTY
displays with 8 or fewer colors, the default is red to blue with
all other colors between (excluding black and white)."
  :type 'alist
  :group 'dvc)

(defcustom dvc-annotate-very-old-color "#3F3FFF"
  "Color for lines older than the current color range in \\[dvc-annotate]]."
  :type 'string
  :group 'dvc)

(defcustom dvc-annotate-background "black"
  "Background color for \\[dvc-annotate].
Default color is used if nil."
  :type 'string
  :group 'dvc)

(defcustom dvc-annotate-face-misc-attribute '((:weight . bold))
  "Other face attribute for faces used in dvc annotation.
Specify them as alist of (attribute . value) or nil to ignore."
  :type 'string
  :group 'dvc)


;;
;; -------------------------------------------------
;;

(defun dvc-annotate-oldest-in-map (color-map)
  "Return the oldest time in the COLOR-MAP."
  ;; Since entries should be sorted, we can just use the last one.
  (caar (last color-map)))

(defun dvc-annotate-display-autoscale (&optional full)
  "Highlight the output of \\[dvc-annotate] using an autoscaled color map.
Autoscaling means that the map is scaled from the current time to the
oldest annotation in the buffer, or, with prefix argument FULL, to
cover the range from the oldest annotation to the newest."
  (interactive "P")
  (let ((newest 0.0)
        (oldest 999999.)               ;Any CVS users at the founding of Rome?
        (current (dvc-annotate-convert-time (current-time)))
        date)
    (message "Redisplaying annotation...")
    ;; Run through this file and find the oldest and newest dates annotated.
    (save-excursion
      (goto-char (point-min))
      (while (setq date (prog1 (dvc-annotate-time)
                          (forward-line 1)))
        (if (> date newest)
            (setq newest date))
        (if (< date oldest)
            (setq oldest date))))
    (dvc-annotate-display
     (/ (- (if full newest current) oldest)
        (dvc-annotate-oldest-in-map dvc-annotate-color-map))
     (if full newest))
    (message "Redisplaying annotation...done \(%s\)"
             (if full
                 (format "Spanned from %.1f to %.1f days old"
                         (- current oldest)
                         (- current newest))
               (format "Spanned to %.1f days old" (- current oldest))))))

;;
;; -------------------------------------------------
;;

(defun dvc-annotate-compcar (threshold a-list)
  "Test successive cons cells of A-LIST against THRESHOLD.
Return the first cons cell with a car that is not less than THRESHOLD,
nil if no such cell exists."
  (let ((i 1)
        (tmp-cons (car a-list)))
    (while (and tmp-cons (< (car tmp-cons) threshold))
      (setq tmp-cons (car (nthcdr i a-list)))
      (setq i (+ i 1)))
    tmp-cons))                          ; Return the appropriate value

(defun dvc-annotate-convert-time (time)
  "Convert a time value to a floating-point number of days.
The argument TIME is a list as returned by `current-time' or
`encode-time', only the first two elements of that list are considered."
  (/ (+ (* (float (car time)) (lsh 1 16)) (cadr time)) 24 3600))

(defun dvc-annotate-difference (&optional offset)
  "Return the time span in days to the next annotation.
This calls the backend function annotate-time, and returns the
difference in days between the time returned and the current time,
or OFFSET if present."
  (let ((next-time (dvc-annotate-time)))
    (if next-time
        (- (or offset
               (dvc-annotate-current-time))
           next-time))))

(defun dvc-default-annotate-current-time ()
  "Return the current time, encoded as fractional days."
  (dvc-annotate-convert-time (current-time)))

(defvar dvc-annotate-offset nil)

(defun dvc-annotate-display (ratio &optional offset)
  "Highlight `dvc-annotate' output in the current buffer.
RATIO, is the expansion that should be applied to `dvc-annotate-color-map'.
The annotations are relative to the current time, unless overridden by OFFSET."
  (if (/= ratio 1.0)
      (set (make-local-variable 'dvc-annotate-color-map)
           (mapcar (lambda (elem) (cons (* (car elem) ratio) (cdr elem)))
                   dvc-annotate-color-map)))
  (set (make-local-variable 'dvc-annotate-offset) offset)
  (font-lock-mode 1))

(defun dvc-annotate-lines (limit)
  (let (difference)
    (while (and (< (point) limit)
                (setq difference (dvc-annotate-difference dvc-annotate-offset)))
      (let* ((color (or (dvc-annotate-compcar difference dvc-annotate-color-map)
                        (cons nil dvc-annotate-very-old-color)))
             ;; substring from index 1 to remove any leading `#' in the name
             (face-name (concat "dvc-annotate-face-"
                                (if (string-equal
                                     (substring (cdr color) 0 1) "#")
                                    (substring (cdr color) 1)
                                  (cdr color))))
             ;; Make the face if not done.
             (face (or (intern-soft face-name)
                       (let ((tmp-face (make-face (intern face-name))))
                         (set-face-foreground tmp-face (cdr color))
                         (if dvc-annotate-background
                             (set-face-background tmp-face
                                                  dvc-annotate-background))
                         (if (and (not (featurep 'xemacs))
                                  dvc-annotate-face-misc-attribute)
                             (dolist (attr dvc-annotate-face-misc-attribute)
                               (set-face-attribute tmp-face nil
                                                   (car attr) (cdr attr))))
                         tmp-face)))    ; Return the face
             (point (point)))
        (forward-line 1)
        (put-text-property point (point) 'face face)))
    ;; Pretend to font-lock there were no matches.
    nil))

(defun dvc-annotate-time ()
  (dvc-call "dvc-annotate-time"))

(provide 'dvc-annotate)
