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

(defun window (vec start &optional end)
  "As SUBSEQ, but return a displaced vector instead of copying."
  (sunless end (setf it (length vec)))
  (make-array (- end start)
              :displaced-to vec
              :displaced-index-offset start))

(defun windows (vec length)
  "Return a list of all overlapping subsequences length LENGTH of
VEC."
  (loop for i upto (- (length vec) length)
     collecting (window vec i (+ i length))))

(defgeneric g-rest (sequence)
  (:documentation "Return a sequence containing all of the elements of
  SEQUENCE except the first."))
(defmethod g-rest ((sequence list)) (cdr sequence))
(defmethod g-rest ((sequence vector)) (window sequence 1))

(defun map-into-each (result-sequences function &rest sequences)
  "As MAP-INTO, except FUNCTION is expected to return as many values
as there are RESULT-SEQUENCES and each position in each of
RESULT-SEQUENCES is set to the corresponding return value."
  (do ((results (copy-seq result-sequences)
                (map-into results #'g-rest results))
       (sequences sequences (mapcar #'g-rest sequences)))
      ((some (lambda (s) (= 0 (length s))) results) result-sequences)
    (funcall #'map
             nil
             (lambda (s v) (setf (elt s 0) v))
             results
             (multiple-value-list
              (apply function (mapcar (lambda (s) (elt s 0)) sequences))))))

(defun elt+ (sequence &rest offsets)
  "Get the element of SEQUENCE at the index obtained by summing
OFFSETS."
  (elt sequence (apply #'+ offsets)))

;; Implementation taken from
;; https://en.wikipedia.org/wiki/Spline_(mathematics)
(defun cubic-spline-coeffs (xs ys)
  "Calculate the coefficients for a cubic spline with X values
specified by XS and the corresponding Y values by YS, returning a list
of cubic polynomials where each polynomial a + b(x - xi) + c(x - xi)^2 +
d(x - xi)^3 is denoted by a list (A B C D XI)."
  (let* ((k (- (length ys) 1))
         (len (1+ k))
         (a (make-array `(,len) :initial-contents ys))
         (b (make-array `(,k)))
         (d (make-array `(,k)))
         (mu (aprog1 (make-array `(,k))
               (setf (elt it 0) 0)))
         (h (map 'simple-vector
                 (lambda (xs) (- (elt xs 1) (elt xs 0)))
                 (windows xs 2)))
         (alpha (map 'simple-vector
                     (lambda (hs as) (- (* 3
                                           (/ (elt hs 1))
                                           (- (elt as 2) (elt as 1)))
                                        (* 3
                                           (/ (elt hs 0))
                                           (- (elt as 1)
                                              (elt as 0)))))
                     (windows h 2)
                     (windows a 3)))
         (c (aprog1 (make-array `(,len))
              (setf (elt it k) 0)))
         (l (aprog1 (make-array `(,len))
              (setf (elt it 0) 1)
              (setf (elt it k) 1)))
         (z (aprog1 (make-array `(,len))
              (setf (elt it 0) 0)
              (setf (elt it k) 0))))
    (loop for i from 1 to (- k 1)
       do
         (setf (elt l i) (- (* 2 (- (elt+ xs i 1) (elt+ xs i -1)))
                            (* (elt+ h i -1) (elt+ mu i -1))))
         (setf (elt mu i) (/ (elt h i) (elt l i)))
         (setf (elt z i) (/ (- (elt+ alpha i -1) (* (elt+ h i -1) (elt+ z i -1)))
                            (elt l i))))
    (loop for j downfrom (- k 1) to 0
       do
         (setf (elt c j) (- (elt z j) (* (elt mu j) (elt+ c j 1))))
         (setf (elt b j) (- (/ (- (elt+ a j 1) (elt a j))
                               (elt h j))
                            (/ (* (elt h j) (+ (elt+ c j 1) (* 2 (elt c j))))
                               3)))
         (setf (elt d j) (/ (- (elt+ c j 1) (elt c j)) (* 3 (elt h j)))))
    (map 'list #'list a b c d xs)))

(defun cubic-spline-body (x xs ys)
  "Generate the body of a function which will interpolate between the
points specified by XS and YS using cubic splines, with X as the
interpolation input."
  (flet ((cubic (x ai bi ci di xi)
           (let ((ti `(- ,x ,xi)))
             `(+ ,ai
                 (* ,bi ,ti)
                 (* ,ci (expt ,ti 2))
                 (* ,di (expt ,ti 3))))))
    `(cond
       ,@(loop
            for poly in (cubic-spline-coeffs xs ys)
            for max across (window xs 1)
            collecting `((< ,x ,max) ,(apply #'cubic x poly))))))

(defun cubic-spline-function (xs ys)
  "Generate a function which interpolates between the points specified
by XS and YS using cubic splines."
  (let ((x (make-symbol "X")))
    (compile nil `(lambda (,x) ,(cubic-spline-body x xs ys)))))
