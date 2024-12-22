;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               sublist.lisp
;;;;
;;;;   Started:            Wed Jun 15 14:04:14 2022
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

;; (defpackage :sublist (:use :common-lisp :lang :test))

;; (in-package :sublist)

(defpackage :sublist
  (:use :cl)
  (:export :sublist))

(in-package :sublist)

(defun sublist (list1 list2)
  "what is list1 of list2 (sublist, superlist, equal or unequal)"
  (let ((list1-super-p (superlist list1 list2))
        (list2-super-p (superlist list2 list1)))
       (cond ((and list1-super-p list2-super-p) :equal)
         (list1-super-p :superlist)
         (list2-super-p :sublist)
         (t :unequal))))

;; (defun superlist (a b)
;;   (cond ((endp b) t)
;;     ((endp a) nil)
;;     ((eql (first a) (first b)) (superlist (rest a) (rest b)))
;;     (t (superlist (rest a) b))))
(defun superlist (a b)
  (cond ((endp b) t)
        ((endp a) nil)
        (t (or (run a b)
               (superlist (rest a) b)))) )

(defun run (a b)
  (cond ((endp b) t)
        ((endp a) nil)
        ((eql (first a) (first b)) (run (rest a) (rest b)))
        (t nil)))

;;;
;;;    Cheating???
;;;    
;; (defun sublist (list1 list2)
;;   "What is list1 of list2 (sublist, superlist, equal or unequal)"
;;   (cond ((equal list1 list2) :equal)
;;         ((search list1 list2) :sublist)
;;         ((search list2 list1) :superlist)
;;         (t :unequal)))
