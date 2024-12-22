#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               strain.lisp
;;;;
;;;;   Started:            Tue Dec 10 12:31:03 2024
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
(defpackage :strain
  (:use :cl)
  (:export :keep :discard))

(in-package :strain)

(defun keep (f elts)
  (cond ((endp elts) '())
        ((funcall f (first elts)) (cons (first elts) (keep f (rest elts))))
        (t (keep f (rest elts)))) )

(defun keep (f elts)
  (loop for elt in elts when (funcall f elt) collect elt))

(defun discard (f elts)
  (keep (complement f) elts))
