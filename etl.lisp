#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   APL is like a perfect diamond: if you add anything to it, it becomes flawed. In contrast, Lisp is like a bean bag--you can sit on a bean bag and squash it, but it will always rise again.
;;;;   -- Joel Moses (attributed)
;;;;
;;;;   Name:               etl.lisp
;;;;
;;;;   Started:            Mon Dec  9 19:28:15 2024
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
(defpackage :etl
  (:use :cl)
  (:export :transform))

(in-package :etl)

(defun transform (data)
  "Transforms hash values into keys with their keys as their values."
  (loop with result = (make-hash-table)
        for score being the hash-keys in data using (hash-value chs)
        do (loop for ch in chs
                 do (setf (gethash (char-downcase ch) result) score))
        finally (return result)))
