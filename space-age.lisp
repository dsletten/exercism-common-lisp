;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               space-age.lisp
;;;;
;;;;   Started:            Sun Jun  5 19:31:19 2022
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

(defpackage :space-age (:use :common-lisp :lang :test))

(in-package :space-age)

(defun round-hundreds (x)
  (/ (round (* x 100)) 1d2))

(defclass planet ()
  ((name :reader name :initarg :name)
   (period :reader period :initarg :period)))

(defmacro defsolar-system (planets)
  (let ((constants (loop for (name period) in planets
                         collect `(defconstant ,name (make-instance 'planet
                                                                    :name (string-capitalize (symbol-name ',name))
                                                                    :period ,period))))
        (functions (loop for (name nil) in planets
                         collect `(defun ,(intern (string-upcase (format nil "on-~A" (symbol-name name)))) (n)
                                    (round-hundreds (/ n 3652425/10000 60 60 24 (period ,name)))) )))
    `(progn ,@constants
            ,@functions)))

(defsolar-system ((mercury 0.2408467d0)
                  (venus 0.61519726d0)
                  (earth 1d0)
                  (mars 1.8808158d0)
                  (jupiter 11.862615d0)
                  (saturn 29.447498d0)
                  (uranus 84.016846d0)
                  (neptune 164.79132d0)))
