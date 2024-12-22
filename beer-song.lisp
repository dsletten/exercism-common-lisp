;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               beer-song.lisp
;;;;
;;;;   Started:            Tue Jun 21 21:45:56 2022
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
(defpackage :beer-song (:use :common-lisp))

(in-package :beer-song)

(defgeneric verse (n)
  (:documentation "Recite a verse for N bottles."))
(defmethod verse ((n integer))
  (format nil "~D bottle~:P of beer on the wall, ~:*~D bottle~:P of beer.~%Take ~:*~[~;it~:;one~] down and pass it around, ~[no more~:;~:*~D~] bottle~:P of beer on the wall.~%" n (1- n)))
(defmethod verse ((n (eql 0)))
  (declare (special start))
  ;;    The test suite is incorrect. `start` bottles should be purchased once all of the beer has been consumed rather than
  ;;     hardwiring 99.
  (format nil "No more bottles of beer on the wall, no more bottles of beer.~%Go to the store and buy some more, ~D bottle~:P of beer on the wall.~%" start))
;  (format nil "No more bottles of beer on the wall, no more bottles of beer.~%Go to the store and buy some more, ~D bottle~:P of beer on the wall.~%" 99))

;;;
;;;    Common Lisp exercise...lazy. Extraneous empty line at end.
;;;    
(defun sing (start &optional (end 0))
  (declare (special start))
  (with-output-to-string (beer)
    (labels ((sing-verse (n)
               (when (>= n end)
                 (write-line (verse n) beer)
                 (sing-verse (1- n)))) )
      (sing-verse start))))

;;;
;;;    No final empty line!
;;;    
(defun sing-javascript (start &optional (end 0))
  (declare (special start))
  (with-output-to-string (beer)
    (labels ((sing-one-verse (n)
               (when (>= n end)
                 (write-string (verse n) beer)
                 (sing-more-verses (1- n))))
             (sing-more-verses (n)
               (when (>= n end)
                 (terpri beer)
                 (write-string (verse n) beer)
                 (sing-more-verses (1- n)))))
      (sing-one-verse start))))

