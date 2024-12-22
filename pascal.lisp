#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a bean bag--you can sit on a bean bag and squash it, but it will always rise again.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               pascal.lisp
;;;;
;;;;   Started:            Fri Dec  6 22:52:33 2024
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
;;;;   Notes: 见 SICP ch. 1 (2012)
;;;;
;;;;
(load "/home/slytobias/lisp/packages/core.lisp")
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :pascal (:use :common-lisp :core :test))

(in-package :pascal)

(defun rows (n)
  (labels ((pascal (i)
             (do* ((c 0 (1+ c))
                   (v 1 (* v (/ (- i c) c)))
                   (result (cons v '()) (cons v result)))
                  ((= c (1- i)) (nreverse result)))) )
    (loop for i from 1 to n collect (pascal i))))
