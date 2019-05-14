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

(defun setup-alsd ()
  "Set up the necessary parameters for alsd to begin."
  (update-alr)
  (setf *als-interpolation-func* (apply #'cubic-spline-function *alr*)))

(defvar *helper-process*)

(defun stop-helper ()
  (external-program:signal-process *helper-process* :hangup))

(defun start-helper (helper-path)
  (setf *helper-process*
        (external-program:start
         (truename helper-path)
         `(,*control-socket-path*)
         :error t
         :status-hook
         (lambda (proc)
           (declare (ignore proc))
           (format *error-output* "Warning: helper process exited~%"))))
  (exit-hooks:add-exit-hook #'stop-helper))

(defun start-daemon (helper-path)
  "Do everything required to start up the daemon."
  (setup-alsd)
  (update-screen)
  (handle-ipc (lambda () (start-helper helper-path))))

(defun entry-point ()
  "Parse the command-line arguments and start the daemon; at present,
only the first argument (the path to the helper) is used."
  (setf uiop/image:*lisp-interaction* nil)
  (handler-bind
      ((serious-condition #'(lambda (c)
                              (uiop/image:die -1 "~a" c))))
    (let ((args (uiop:command-line-arguments)))
      (unless (= (length args) 1)
        (error
         "Incorrect number of arguments~@
          Usage: ~a <helper-path>~%"
         (uiop/image:argv0)))
      (start-daemon (parse-namestring (nth 1 args))))))
