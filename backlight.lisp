;; This file is part of alsd.

;; alsd is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; alsd is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with alsd. If not, see <https://www.gnu.org/licenses/>.

(in-package :alsd)

(defparameter *ali-path*
  "/sys/bus/acpi/devices/ACPI0008:00/als_bus/ali")
(defparameter *alr-path*
  "/sys/bus/acpi/devices/ACPI0008:00/als_bus/alr")
(defparameter *backlight-path*
  "/sys/class/backlight/intel_backlight/brightness")
(defparameter *max-backlight-path*
  "/sys/class/backlight/intel_backlight/max_brightness")

(defparameter *hex-digits*
  (loop for i from 0 to 15 collecting (elt (write-to-string i :base 16) 0)))
(defun hex-digit-p (char)
  (member (char-upcase char) *hex-digits*))

(defun read-acpi-package (stream)
  "Read an ACPI package representation from STREAM. Note that this
supports any of the usual Lisp syntax, only modifying the current
readtable enough to make ACPI package representations parse; thus, it
should NOT be used on untrusted data without prior readtable
modification. We use it here because there are bigger problems than
the takeover of a backlight daemon if sysfs is not trusted."
  (let ((*readtable* (copy-readtable))
        (*read-base* 16))
    ;; Define the square-bracket characters as list delimiters
    (set-macro-character #\[
                         (lambda (stream char)
                           (declare (ignore char))
                           (read-delimited-list #\] stream t)))
    (set-macro-character #\] (get-macro-character #\) nil))
    ;; Define comma as a whitespace character
    (set-syntax-from-char #\, #\Space)
    ;; Allow the 0x hex numbers to work; make this non-terminating so
    ;; it doesn't interrupt the numeric tokens when a hex number
    ;; contains 0
    (set-macro-character #\0
                         (lambda (stream char)
                           (if (eql (peek-char nil stream nil nil) #\x)
                               (read-char stream)
                               (unread-char char stream))
                           (let ((*readtable* (copy-readtable)))
                             ;; Prevent zeroes from being interpreted
                             ;; when reading something after 0x;
                             ;; 0x0xab is not valid
                             (set-syntax-from-char #\0 #\1)
                             (read stream)))
                         t)
    (read stream)))

(defun read-alr ()
  "Reads the ALR table and returns it as a two-item list, containing
first a vector of ALS readings, then a vector of their corresponding
backlight adjustment percentages."
  (->> (with-open-file (alr-file *alr-path*)
         (read-acpi-package alr-file))
       ;; Rotate the 2d list 90 degrees, turning the list of points
       ;; into the two vectors
       (apply #'mapcar #'vector)
       ;; Reverse their order; bafflingly, the ACPI spec seems to
       ;; treat backlight as the manipulated and ALI as the responding
       ;; variable, including in the ordering of ALR entries.
       nreverse))

;;; TODO: Either remove *alr* and update-alr altogether, or make them
;;; update *als-interpolation-func* appropriately somehow.

(defvar *alr* nil)

(defun update-alr ()
  "Reads the ALR table and updates the stored value."
  (setf *alr* (read-alr)))

(defvar *als-interpolation-func* nil)

(defvar *max-backlight* nil)

(defun max-backlight ()
  "Gets the maximum backlight value or returns the cached value for
it."
  (or
   *max-backlight*
   (setf *max-backlight*
         (with-open-file (max-backlight *max-backlight-path*)
           (parse-integer (read-line max-backlight))))))

(defun set-backlight (value)
  "Set the backlight brightness to the given value."
  (with-open-file (backlight-file *backlight-path*
                                  :direction :output
                                  :if-exists :append)
    (->> value
         round
         (max 0)
         (min (max-backlight))
         (format backlight-file "~a")))
  nil)

(defun get-ali ()
  "Read an illuminance value from *ALI-PATH*."
  (with-open-file (ali-file *ali-path*)
    (read ali-file)))

(defun update-backlight (ali-reading desired-brightness)
  "Update the backlight based on the given illuminance reading, using
*als-interpolation-func* to get the backlight adjustment value."
  ;; TODO: Adjust based on the user's desired brightness, not based on
  ;; the maximum
  (let ((adjustment (/ (funcall *als-interpolation-func* ali-reading) 100)))
    (set-backlight (* desired-brightness adjustment))))
