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
  "/sys/bus/acpi/devices/ACPI0008:00/als_bus/ali"
  "The path to a file which can be used to read the current
  illuminance from the ambient light sensor.")
(defparameter *alr-path*
  "/sys/bus/acpi/devices/ACPI0008:00/als_bus/alr"
  "The path to a file which can be used to read the current response
  table from the ambient light sensor.")
(defparameter *backlight-path*
  "/sys/class/backlight/intel_backlight/brightness"
  "The path to a file which can be used to read or write the backlight
  brightness.")
(defparameter *max-backlight-path*
  "/sys/class/backlight/intel_backlight/max_brightness"
  "The path to a file which can be used to read the maximum possible
  backlight brightness.")

(defun read-acpi-package (stream)
  "Read an ACPI package representation from STREAM, where each package
is a list of comma-separated integers and other packages delimited by
square brackets."
  (let ((char (peek-char nil stream)))
   (cond
     ((char= char #\[)
      (read-char stream)
      (loop with next-char
            collect (read-acpi-package stream)
            do (setf next-char (read-char stream))
            until (char= next-char #\])
            do (assert (char= next-char #\,)
                       ()
                       "Expected comma separating package elements, found ~S"
                       next-char)
               (peek-char t stream)))
     ((digit-char-p char)
      (loop for next-char = char then (peek-char nil stream nil)
            for next-digit = (and next-char (digit-char-p next-char))
            while next-digit
            for num = next-digit then (+ (* num 10) next-digit)
            ;; Only remove the character from the stream if it is a
            ;; digit
            do (read-char stream)
            finally (return num)))
     (t
      (error "Expecting package literal or integer, found ~S" char)))))

(defun read-alr ()
  "Read the ALR table and return it as a two-item list, containing
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

(defvar *alr* nil
  "The current ambient light response table, stored as (XS YS), where
  XS contains illuminance values and YS contains their corresponding
  backlight values.")

(defun update-alr ()
  "Read the ALR table and update the stored value."
  (setf *alr* (read-alr)))

(defvar *als-interpolation-func* nil
  "The function used to convert illuminance values to backlight
  values, calculated via *ALR*.")

(defvar *max-backlight* nil
  "The cached maximum backlight value. (MAX-BACKLIGHT) should be used
  instead so this value can be initialized if necessary.")

(defun max-backlight ()
  "Get the maximum backlight value or return the cached value for
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
*ALS-INTERPOLATION-FUNC* to get the backlight adjustment value."
  ;; TODO: Adjust based on the user's desired brightness, not based on
  ;; the maximum
  (let ((adjustment (/ (funcall *als-interpolation-func* ali-reading) 100)))
    (set-backlight (* desired-brightness adjustment))))
