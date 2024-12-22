#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a ball of mud--if you add more to it, you get a bigger ball of mud.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               nucleotide-count.lisp
;;;;
;;;;   Started:            Sun Dec  8 17:40:45 2024
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
(defpackage :nucleotide-count
  (:use :cl)
  (:export :dna-count :nucleotide-counts :invalid-nucleotide))

(in-package :nucleotide-count)

(define-condition invalid-nucleotide (error) ())

(defun result-hash (as cs gs ts)
  (let ((result (make-hash-table)))
    (setf (gethash #\A result) as
          (gethash #\C result) cs
          (gethash #\G result) gs
          (gethash #\T result) ts)
    result))

(defun dna-count (nucleotide strand)
  "Returns a count of the given nucleotide appearing in a DNA strand."
  (unless (find nucleotide "ACGT" :test #'char-equal)
    (error (make-condition 'invalid-nucleotide)))
  (loop for n across strand
        count (char-equal n nucleotide)))

(defun nucleotide-counts (strand)
  "Returns a hash of nucleotides and their counts in a given DNA strand."
  (loop for nucleotide across strand
        count (char-equal #\A nucleotide) into as
        count (char-equal #\C nucleotide) into cs
        count (char-equal #\G nucleotide) into gs
        count (char-equal #\T nucleotide) into ts
        finally (return (result-hash as cs gs ts))))
