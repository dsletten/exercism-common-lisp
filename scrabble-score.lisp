#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               scrabble-score.lisp
;;;;
;;;;   Started:            Mon Dec  9 19:42:12 2024
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
(defpackage :scrabble-score
  (:use :cl)
  (:export :score-word))

(in-package :scrabble-score)

(defun score-tile (tile)
  (case (char-downcase tile)
    ((#\a #\e #\i #\o #\u #\l #\n #\r #\s #\t) 1)
    ((#\d #\g) 2)
    ((#\b #\c #\m #\p) 3)
    ((#\f #\h #\v #\w #\y) 4)
    (#\k 5)
    ((#\j #\x) 8)
    ((#\q #\z) 10)
    (otherwise 0)))

(defun score-word (word)
  "Computes the score for an entire word."
  (reduce #'+ (map 'list #'score-tile word)))
