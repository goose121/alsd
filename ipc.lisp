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

(defparameter *control-socket-name* "alsd-ctl")

(defvar *user-brightness-percent* 100)

(defun update-screen ()
  (update-backlight (get-ali)
                    (* (max-backlight)
                       (/ *user-brightness-percent* 100))))

(defun handle-client (client)
  (let ((req (uiop:with-safe-io-syntax (:package :alsd)
               (read client))))
    ;; TODO: improve error message
    (check-type req list)
    (format
     client
     "~A"
     (case (first req)
       (update-screen t)
       (adjust-brightness
        (incf *user-brightness-percent* (second req)))
       ((stop exit quit) (throw 'exit (values)))
       (otherwise (error "Invalid IPC operation ~s" (first req)))))
    (update-screen)))

(defmacro with-bound-socket ((socket &rest args) &body body)
  `(unwind-protect
        (progn
          (bind-address ,socket (ensure-address ,@args))
          ,@body)
     (close ,socket)))

(defun handle-ipc (run-with-socket)
  "Handle IPC requests in a loop. Once the socket is open, run the
function RUN-WITH-SOCKET."
  ;; Cache the max backlight value because the cache isn't thread-safe
  (max-backlight)
  (catch 'exit
    (let ((ctl-socket (make-socket :address-family :local
                                   :type :stream
                                   :connect :passive)))
      (with-bound-socket (ctl-socket *control-socket-name*
                                     :family :local
                                     :abstract t)
        (listen-on ctl-socket :backlog 5)
        (funcall run-with-socket)
        (loop
           (handler-case
               (let ((client (accept-connection ctl-socket :wait t)))
                 (unwind-protect (handle-client client)
                   (finish-output client)
                   (shutdown client :read t :write t)
                   (close client)))
             (error (err) (format *error-output* "~A~%" err))))))))
