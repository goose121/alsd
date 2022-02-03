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

(defsystem "alsd"
  :description "A daemon to use ambient light sensor values to adjust
  the backlight brightness on Linux"
  :version "0.0.1"
  :author "goose121 (https://github.com/goose121)"
  :license "GPLv3-or-later"
  :depends-on (#:external-program
               #:exit-hooks)
               #:anaphora
               #:iolib
               #:bordeaux-threads
               #:unix-opts
               #:cl-log
  :components ((:file "packages")
               (:file "log-defs")
               (:file "cubic-interp" :depends-on ("packages"))
               (:file "backlight" :depends-on ("packages"))
               (:file "ipc" :depends-on ("backlight" "log-defs" "packages"))
               (:file "main" :depends-on
                      ("cubic-interp" "backlight" "log-defs" "ipc" "packages")))
  :build-operation "program-op" ;; leave as is
  :build-pathname "alsd"
  :entry-point "alsd:entry-point")
