;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               difference-of-squares.lisp
;;;;
;;;;   Started:            Mon Jun  6 14:49:57 2022
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/lang.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :difference-of-squares (:use :common-lisp :lang :test))

(in-package :difference-of-squares)

(defpackage :difference-of-squares
  (:use :cl)
  (:export :sum-of-squares
           :square-of-sum
           :difference))

(in-package :difference-of-squares)

(defun square-of-sum (n)
  "Calculates the square of the sum for a given number."
  (expt (reduce #'+ (loop for i from 1 to n collect i)) 2))

(defun sum-of-squares (n)
  "Calculates the sum of squares for a given number."
;  (reduce #'+ (mapcar #'(lambda (x) (* x x)) (loop for i from 1 to n collect i))))
  (reduce #'+ (loop for i from 1 to n collect (* i i))))

(defun square-of-sum-loop (n)
  "Calculates the square of the sum for a given number."
  (expt (loop for i from 1 to n summing i) 2))

(defun sum-of-squares-loop (n)
  "Calculates the sum of squares for a given number."
  (loop for i from 1 to n summing (* i i)))

(defun difference (n)
  "Finds the diff. between the square of the sum and the sum of the squares."
  (- (square-of-sum n) (sum-of-squares n)))

(defun sum-of-squares (n)
  (/ (* n (1+ n) (1+ (* n 2))) 6))

(defun square-of-sum (n)
  (let ((n1 (1+ n)))
    (/ (* n n n1 n1) 4)))

;; //
;; //    User sugrado
;; //    
;; /* unsigned int sum_of_squares(unsigned int n) { */
;; /*   return n*(n+1)*(2*n+1)/6; */
;; /* } */

;; /* unsigned int square_of_sum(unsigned int n) { */
;; /*   return n*(n+1)*n*(n+1)/4; */
;; /* } */
        
;; My "improvement" actually takes 7 operations rather than 6!
;;     int computeSumOfSquaresTo(int n) {
;;         int n2 = n * n;
;;         int n3 = n2 * n;
;;         return (2 * n3 + 3 * n2 + n) / 6;
;;     }

