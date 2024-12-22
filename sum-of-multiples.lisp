;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               sum-of-multiples.lisp
;;;;
;;;;   Started:            Sat Dec 14 19:27:36 2024
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
(defpackage :sum-of-multiples
  (:use :cl)
  (:export :sum))

(in-package :sum-of-multiples)

(defun summation (n)
  (* n (1+ n) 1/2))

(defun factor-sum (factor limit)
;  (let ((n (floor (1- limit) factor)))
  (let ((n (ceiling (- limit factor) factor)))
    (* factor (summation n))))

;;;
;;;    This is correct except for multiple factors:
;;;    (sum '(5 6 8) 150)
;;;    (sum '(2 3 5 7 11) 10000)
;;;    
(defun sum (factors limit)
  (let ((factors* (remove 0 factors :test #'=)))
    (case (length factors*)
      (0 0)
      (1 (factor-sum (first factors*) limit))
      (otherwise (- (reduce #'+ (mapcar #'(lambda (factor) (factor-sum factor limit)) factors*))
                    (factor-sum (apply #'lcm factors*) limit)))) ))
;                    (* (length factors*) (factor-sum (apply #'lcm factors*) limit)))) )))

;;;
;;;    The duplicates to remove get tricky with multiple factors...
;;;    
;; (defun factor-list (n limit)
;;   (loop for factor from n below limit by n collect factor))

;; ? (factor-list 5 150)
;; (5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90 95 100 105 110 115 120 125 130 135 140 145)
;; ? (factor-list 6 150)
;; (6 12 18 24 30 36 42 48 54 60 66 72 78 84 90 96 102 108 114 120 126 132 138 144)
;; ? (factor-list 8 150)
;; (8 16 24 32 40 48 56 64 72 80 88 96 104 112 120 128 136 144)
;; ? (factor-list (lcm 5 6 8) 150)
;; (120)
;; ? (factor-list (lcm 5 6) 150)
;; (30 60 90 120)
;; ? (factor-list (lcm 5 8) 150)
;; (40 80 120)
;; ? (factor-list (lcm 6 8) 150)
;; (24 48 72 96 120 144)




(defun sum (factors limit)
  (labels ((factor-list (n)
             (loop for factor from n below limit by n collect factor)))
    (reduce #'+ (remove-duplicates (apply #'append (mapcar #'factor-list (remove 0 factors :test #'=)))) )))
