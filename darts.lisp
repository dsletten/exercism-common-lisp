;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               darts.lisp
;;;;
;;;;   Started:            Thu Dec  5 18:23:43 2024
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
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :darts (:use :common-lisp :core :test))

(in-package :darts)

(defpackage :darts
  (:use :cl)
  (:export :score))

(in-package :darts)

(defun score (x y)
  (let ((r (abs (complex x y))))
    (cond ((<= r 1) 10)
          ((<= r 5) 5)
          ((<= r 10) 1)
          (t 0))))
