;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               bob.lisp
;;;;
;;;;   Started:            Sun Dec  8 20:36:50 2024
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
(defpackage :bob
  (:use :cl)
  (:export :response))
(in-package :bob)

(defun questionp (s)
  (char= #\? (char s (1- (length s)))) )

(defun yelledp (s)
  (and (some #'alpha-char-p s)
       (notany #'lower-case-p s)))

(defun silentp (s) (string= "" s))

(defclass response () ())
(defclass question-response (response) ())
(defclass yelled-response (response) ())
(defclass yelled-question-response (yelled-response question-response) ())
(defclass silent-treatment (response) ())

(defgeneric respond (response))
(defmethod respond ((response response))
  (declare (ignore response))
  "Whatever.")
(defmethod respond ((response question-response))
  (declare (ignore response))
   "Sure.")
(defmethod respond ((response yelled-response))
  (declare (ignore response))
  "Whoa, chill out!")
(defmethod respond ((response yelled-question-response))
  (declare (ignore response))
  "Calm down, I know what I'm doing!")
(defmethod respond ((response silent-treatment))
  (declare (ignore response))
  "Fine. Be that way!")

(defun choose-response (trimmed)
  (cond ((silentp trimmed) (make-instance 'silent-treatment))
        ((and (questionp trimmed) (yelledp trimmed)) (make-instance 'yelled-question-response))
        ((questionp trimmed) (make-instance 'question-response))
        ((yelledp trimmed) (make-instance 'yelled-response))
        (t (make-instance 'response))))

(defun response (hey-bob)
  (respond (choose-response (string-trim '(#\Space #\Tab #\Newline) hey-bob))))

