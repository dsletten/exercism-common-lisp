#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a bean bag--you can sit on a bean bag and squash it, but it will always rise again.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               anagram.lisp
;;;;
;;;;   Started:            Fri Dec  6 21:58:10 2024
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
(defpackage :anagram
  (:use :cl)
  (:export :anagrams-for))

(in-package :anagram)

(defun anagramp (word candidate)
  (string-equal (sort (copy-seq word) #'char-lessp)
                (sort (copy-seq candidate) #'char-lessp)))

(defun anagrams-for (subject candidates)
  "Returns a sublist of candidates which are anagrams of the subject."
  (remove-if-not #'(lambda (candidate) (anagramp subject candidate)) (remove subject candidates :test #'string-equal)))

;; (test handles-case-of-greek-letters
;;   (is (equal '("ΒΓΑ" "γβα") (anagram:anagrams-for "ΑΒΓ" '("ΒΓΑ" "ΒΓΔ" "γβα" "αβγ")))))
