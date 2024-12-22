;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               logans-numeric-partition.lisp
;;;;
;;;;   Started:            Wed Jul 13 00:32:01 2022
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

(defpackage :logans-numeric-partition (:use :common-lisp :lang :test))

(in-package :logans-numeric-partition)

(defpackage :logans-numeric-partition
  (:use :cl)
  (:export :categorize-number :partition-numbers))

(in-package :logans-numeric-partition)

(defun categorize-number (buckets n)
  (assert (integerp n) (n) "N must be an integer: ~S" n)
  (destructuring-bind (odds . evens) buckets
    (if (oddp n)
        (cons (cons n odds) evens)
        (cons odds (cons n evens)))))

(defun partition-numbers (ns)
  (reduce #'categorize-number ns :initial-value (cons nil nil)))

(defun partition-numbers (ns)
  (reduce #'(lambda (bins n)
              (if (evenp n)
                  (cons (car bins) (cons n (cdr bins)))
                  (cons (cons n (car bins)) (cdr bins))))
          ns
          :initial-value '(() . ())))

(defun partition-numbers (ns)
  (loop for n in ns
        when (evenp n)
          collect n into evens
        else
          collect n into odds
        end
        finally (return (cons odds evens))))
