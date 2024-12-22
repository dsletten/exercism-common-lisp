;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               larrys-winning-checker.lisp
;;;;
;;;;   Started:            Fri Jun 24 01:27:49 2022
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

(defpackage :larrys-winning-checker (:use :common-lisp :lang :test))

(in-package :larrys-winning-checker)

(defpackage :larrys-winning-checker
  (:use :cl)
  (:export
   :make-empty-board
   :make-board-from-list
   :all-the-same-p
   :row
   :column))

(in-package :larrys-winning-checker)

(defconstant dimension 3)
(defun make-empty-board ()
  (make-board-from-list (loop repeat dimension collect (loop repeat dimension collect nil))))

(defun make-empty-board ()
  (make-board-from-list (make-list 3 :initial-element (make-list 3 :initial-element nil))))

;;;
;;;    Not OK in general, but OK to feed as :initial-contents!
;;;    
;; (defvar *l* (make-list 3 :initial-element nil))
;; (defvar *board* (make-list 3 :initial-element *l*))
;; *board* => ((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL))
;; (setf (second *l*) 'whoops)
;; *l* => (NIL WHOOPS NIL)
;; *board* => ((NIL WHOOPS NIL) (NIL WHOOPS NIL) (NIL WHOOPS NIL))

(defun make-board-from-list (list)
  (make-array (list dimension dimension) :initial-contents list))

(defun all-the-same-p (row-or-col)
  (every #'(lambda (elt) (eq elt (aref row-or-col 0))) row-or-col))

(defun all-the-same-p (row-or-col)
  (let ((target (aref row-or-col 0)))
    (every #'(lambda (elt) (eq elt target)) row-or-col)))

(defun row (board row-num) 
  (let ((v (make-sequence 'vector dimension)))
    (dotimes (j dimension v)
      (setf (aref v j) (aref board row-num j)))) )

(defun row (board row-num) 
  (loop for j below dimension
        collect (aref board row-num j) into result 
        finally (return (coerce result 'vector))))

(defun column (board col-num)
  (let ((v (make-sequence 'vector dimension)))
    (dotimes (i dimension v)
      (setf (aref v i) (aref board i col-num)))) )

(defun column (board col-num)
  (loop for i below dimension 
        collect (aref board i col-num) into result 
        finally (return (coerce result 'vector))))
