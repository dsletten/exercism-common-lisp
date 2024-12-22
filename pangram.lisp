#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               pangram.lisp
;;;;
;;;;   Started:            Fri Dec  6 21:36:55 2024
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
(defpackage :pangram
  (:use :cl)
  (:export :pangramp))

(in-package :pangram)

(defconstant alphabet (loop for i from (char-code #\a) upto (char-code #\z) collect (code-char i)))
(defun pangramp (sentence)
  (loop with letters = (make-hash-table)
        for ch across (remove-if-not #'alpha-char-p (string-downcase sentence))
        do (setf (gethash ch letters) t)
        finally (return (= 26 (hash-table-count letters)))) )

(defun pangramp (sentence)
  (= 26 (length (remove-duplicates (remove-if-not #'alpha-char-p sentence) :test #'char-equal))))

;; (print (pangramp "The quick brown fox jumps over the lazy dog"))
;; (print (pangramp "Five quacking Zephyrs jolt my wax bed."))
