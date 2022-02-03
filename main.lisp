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
  (when (eql :running (external-program:process-status *helper-process*))
    (external-program:signal-process *helper-process* :hangup)))

(defun start-helper (helper-path)
  (setf *helper-process*
        (external-program:start
         (truename helper-path)
         `(,*control-socket-name*)
         :error t
         :status-hook
         (lambda (proc)
           (declare (ignore proc))
           (warn "ALSD helper process exited"))))
  (exit-hooks:add-exit-hook #'stop-helper))

(defun start-daemon (helper-path)
  "Do everything required to start up the daemon."
  (handler-bind
      ((serious-condition
         (lambda (c)
           (cl-log:log-message '(alsd/log:alsd alsd/log:fatal) "~A" c)
           (abort)))
       (warning
         (lambda (c)
           (cl-log:log-message '(alsd/log:alsd alsd/log:warning) "~A" c)
           (muffle-warning c))))
    (setup-alsd)
    (update-screen)
    (handle-ipc
     (lambda () (start-helper helper-path))
     (lambda () (stop-helper)))))

(defun entry-point ()
  "Parse the command-line arguments and start the daemon; at present,
only the first argument (the path to the helper) is used."
  (setf uiop/image:*lisp-interaction* nil)
  (restart-case
      (let ((args (uiop:command-line-arguments)))
        (setf (cl-log:log-manager)
              (make-instance
               'cl-log:log-manager
               :message-class 'cmdline-message))
        (cl-log:start-messenger
         'cl-log:text-stream-messenger
         :stream *error-output*
         :filter 'alsd/log:error)
        (handler-bind
            ((serious-condition
               (lambda (c)
                 (cl-log:log-message '(alsd/log:alsd alsd/log:fatal) "~A" c)
                 (abort))))
          (unless (= (length args) 1)
            (error
             "Incorrect number of arguments~@
              Usage: ~a <helper-path>"
             (uiop/image:argv0)))
          (start-daemon
           (handler-case (parse-namestring (first args))
             (parse-error ()
               (error "Invalid helper path ~A" (first args)))))))
    (abort ()
      :report "Abort the process."
      (uiop/image:quit -1))))
