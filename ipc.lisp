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

(defparameter *control-socket-path*
  "/tmp/alsd.sock")

(defvar *user-brightness-percent* 100)

(defun update-screen ()
  (update-backlight (get-ali)
                    (* (max-backlight)
                       (/ *user-brightness-percent* 100))))

(defun handle-client (client)
  (let ((req (uiop:with-safe-io-syntax (:package :alsd)
               (read client))))
(defun handle-ipc ()
  "Handle IPC requests in a loop."
    ;; TODO: improve error message
    (check-type req list)
    (format
     client
     "~A"
     (case (first req)
       (update-screen t)
       (adjust-brightness
        (incf *user-brightness-percent* (second req)))
       (stop (throw 'exit (values)))
       (otherwise (error "Invalid IPC operation ~s" (first req)))))
    (update-screen)))

  ;; Cache the max backlight value because the cache isn't thread-safe
  (max-backlight)
  (catch 'exit
    (unwind-protect
         (with-open-socket
             (ctl-socket :address-family :local
                         :type :stream
                         :connect :passive
                         :local-filename *control-socket-path*)
           (listen-on ctl-socket :backlog 5)
           (loop
              (handler-case
                  (let ((client (accept-connection ctl-socket :wait t)))
                    (unwind-protect (handle-client client)
                      (finish-output client)
                      (shutdown client :read t :write t)
                      (close client)))
                (error (err) (format *error-output* "~A~%" err)))))
      (delete-file *control-socket-path*))))
