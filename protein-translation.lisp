#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               protein-translation.lisp
;;;;
;;;;   Started:            Tue Dec 10 11:32:48 2024
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
;;;;   Notes: https://en.wikipedia.org/wiki/Translation_(biology)
;;;;
;;;;
(defpackage :protein-translation
  (:use :cl)
  (:export :proteins
           :invalid-protein))

(in-package :protein-translation)

(define-condition invalid-protein (error) ())

(defun proteins (strand)
  (with-input-from-string (in strand)
    (labels ((read-nucleotide ()
               (let ((ch (read-char in nil nil)))
                 (if (null ch)
                     ch
                     (char-upcase ch))))
             (translate (aminos)
               (let ((n (read-nucleotide)))
                 (ecase n
                   ((nil) (stop aminos))
                   (#\A (a aminos))
                   (#\U (u aminos)))) )
             (a (aminos)
               (let ((n (read-nucleotide)))
                 (ecase n
                   (#\U (au aminos)))) )
             (au (aminos)
               (let ((n (read-nucleotide)))
                 (ecase n
                   (#\G (translate (cons "Methionine" aminos)))) ))
             (u (aminos)
               (let ((n (read-nucleotide)))
                 (ecase n
                   (#\A (ua aminos))
                   (#\C (uc aminos))
                   (#\G (ug aminos))
                   (#\U (uu aminos)))) )
             (ua (aminos)
               (let ((n (read-nucleotide)))
                 (ecase n
                   ((#\A #\G) (stop aminos))
                   ((#\C #\U) (translate (cons "Tyrosine" aminos)))) ))
             (uc (aminos)
               (let ((n (read-nucleotide)))
                 (ecase n
                   ((#\A #\C #\G #\U) (translate (cons "Serine" aminos)))) ))
             (ug (aminos)
               (let ((n (read-nucleotide)))
                 (ecase n
                   ((#\A) (stop aminos))
                   ((#\C #\U) (translate (cons "Cysteine" aminos)))
                   ((#\G) (translate (cons "Tryptophan" aminos)))) ))
             (uu (aminos)
               (let ((n (read-nucleotide)))
                 (ecase n
                   ((#\A #\G) (translate (cons "Leucine" aminos)))
                   ((#\C #\U) (translate (cons "Phenylalanine" aminos)))) ))
             (stop (aminos)
               (nreverse aminos)))
      (handler-case (translate '())
        (error ()
          (signal (make-condition 'invalid-protein)))) )))
