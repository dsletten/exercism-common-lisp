#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               phone-number.lisp
;;;;
;;;;   Started:            Wed Dec 11 19:51:31 2024
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
(defpackage :phone-number
  (:use :cl)
  (:export :clean))

(in-package :phone-number)

(defun clean (phrase)
  (handler-case (let ((cleaned (remove #\1 (remove-if-not #'digit-char-p phrase) :end 1)))
                  (cond ((/= (length cleaned) 10) (kaboom))
                        ((< (digit-char-p (char cleaned 0)) 2) (blam))
                        ((< (digit-char-p (char cleaned 3)) 2) (wham))
                        (t cleaned)))
    (error () "0000000000")))
