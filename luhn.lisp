#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               luhn.lisp
;;;;
;;;;   Started:            Thu Dec 12 10:17:45 2024
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
(defpackage :luhn
  (:use :cl)
  (:export :validp))

(in-package :luhn)

(defun validate-digits (digits)
  (let* ((reversed-digits (reverse digits))
;         (transformed-digits (loop for i below (length reversed-digits)
         (transformed-digits (loop for i from 0
                                   for digit in reversed-digits
                                   if (oddp i) collect (multiple-value-bind (q r) (floor digit 5) (+ (* 2 r) q))
                                   else collect digit)))
    (zerop (mod (reduce #'+ transformed-digits) 10))))

(defun validp (input)
  (let* ((trimmed (remove #\space input))
         (digits (loop for ch across trimmed
                       if (digit-char-p ch) collect it
                       else do (return nil))))
    (if (> (length digits) 1)
        (validate-digits digits)
        nil)))

;; (loop for i from 0 to 9 collect (let ((n (* 2 i))) (if (> n 9) (- n 9) n)))
;; (0 2 4 6 8 1 3 5 7 9)

;; (loop for i from 0 to 9 collect (* 2 (mod i 5)))
;; (0 2 4 6 8 0 2 4 6 8)
;; (loop for i from 0 to 9 collect (+ (* 2 (mod i 5)) (floor i 5)))
;; (0 2 4 6 8 1 3 5 7 9)

