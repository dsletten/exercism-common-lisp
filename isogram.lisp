#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a ball of mud--if you add more to it, you get a bigger ball of mud.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               isogram.lisp
;;;;
;;;;   Started:            Tue Dec 10 00:48:28 2024
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
(defpackage :isogram
  (:use :cl)
  (:export :isogram-p))

(in-package :isogram)

(defun isogram-p (string)
  "Is string an Isogram?"
  (let ((trimmed (remove-if #'(lambda (ch) (member ch '(#\- #\space))) string)))
    (= (length trimmed)
       (length (remove-duplicates trimmed :test #'char-equal)))) )

;;;
;;;    This is good enough since REMOVE-DUPLICATES preserves order.
;;;    Removing non ALPHA-CHAR-P chars is adequate as well.
;;;    
(defun isogram-p (string)
  "Is string an Isogram?"
  (let ((trimmed (remove-if #'(lambda (ch) (member ch '(#\- #\space))) string)))
    (string= trimmed
             (remove-duplicates trimmed :test #'char-equal))))
