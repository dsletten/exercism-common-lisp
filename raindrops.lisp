;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               raindrops.lisp
;;;;
;;;;   Started:            Tue Jun 14 11:56:08 2022
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

(defpackage :raindrops (:use :common-lisp :lang :test))

;; (in-package :raindrops)

;; (defpackage :raindrops
;;   (:use :cl)
;;   (:export :convert))

(in-package :raindrops)

(defun convert (n)
  "Converts a number to a string of raindrop sounds."
  (let ((result (with-output-to-string (s)
                  (when (zerop (mod n 3))
                    (write-string "Pling" s))
                  (when (zerop (mod n 5))
                    (write-string "Plang" s))
                  (when (zerop (mod n 7))
                    (write-string "Plong" s)))) )
       (if (string= result "")
           (write-to-string n)
           result)))

;;;
;;;    Inspired by JJ
;;;
(defconstant rain '((3 "Pling")
                    (5 "Plang")
                    (7 "Plong")))
(defun convert (n)
  "Converts a number to a string of raindrop sounds."
  (let ((result (with-output-to-string (s)
                  (loop for (factor sound) in rain
                        when (zerop (mod n factor))
                        do (write-string sound s)))) )
       (if (string= result "")
           (format nil "~D" n)
           result)))
