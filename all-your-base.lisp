;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               all-your-base.lisp
;;;;
;;;;   Started:            Sat Jun 11 22:25:31 2022
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
;;;;   Notes: Tests are inconsistent with corresponding JavaScript exercise.
;;;;          In particular, leading 0's, empty DIGITS list.
;;;;
;;;;  见 ~/lisp/programs/horners.lisp
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :all-your-base (:use :common-lisp :test))

(in-package :all-your-base)

(defun valid-digits-p (digits base)
  (every #'(lambda (digit) (<= 0 digit (1- base))) digits))

(deftest test-valid-digits-p ()
  (check
   (valid-digits-p (loop for i from 0 below 10) 10)
   (not (valid-digits-p '(-1) 8))
   (not (valid-digits-p '(9) 8))))
   
;; (defun evaluate (digits base)
;;   (if (valid-digits-p digits base)
;;       (reduce #'(lambda (sum d) (+ (* sum base) d)) digits :initial-value 0)
;;       nil))

;; (defun evaluate (digits base)
;;   (labels ((evaluate-digits-list (digits result)
;;              (if (null digits)
;;                  result
;;                  (evaluate-digits-list (rest digits)
;;                                        (+ (* result base) (first digits)))) ))
;;     (if (valid-digits-p digits base)
;;         (evaluate-digits-list digits 0)
;;         nil)))

;; (defun evaluate (digits base)
;;   (labels ((evaluate-digits-list (digits result)
;;              (if (null digits)
;;                  result
;;                  (destructuring-bind (d . ds) digits
;;                    (evaluate-digits-list ds (+ (* result base) d)))) ))
;;     (if (valid-digits-p digits base)
;;         (evaluate-digits-list digits 0)
;;         nil)))

(defun evaluate (digits base)
  (reduce #'(lambda (sum d) (+ (* sum base) d)) digits :initial-value 0))

(defun evaluate (digits base)
  (labels ((evaluate-digits-list (digits result)
             (if (null digits)
                 result
                 (evaluate-digits-list (rest digits)
                                       (+ (* result base) (first digits)))) ))
    (evaluate-digits-list digits 0)))

(defun evaluate (digits base)
  (labels ((evaluate-digits-list (digits result)
             (if (null digits)
                 result
                 (destructuring-bind (d . ds) digits
                   (evaluate-digits-list ds (+ (* result base) d)))) ))
    (evaluate-digits-list digits 0)))

(deftest test-evaluate ()
  (check
   (= (evaluate '(0) 10) 0)
   (= (evaluate '(0 0 0) 10) 0)
   (= (evaluate '(0 2 4 6) 10) 246)
   (= (evaluate '(2 4 6) 10) 246)
   (= (evaluate '(1 0 0 0) 2) 8)))
;   (not (evaluate '(15 11 12) 10))))

;;;
;;;    Backwards
;;;    
;; (defun convert (n base)
;;   (cond ((< n base) (list n))
;;         (t (cons (rem n base) (convert (floor n base) base)))))

(defun convert (n base)
  (convert-base n base '()))
(defun convert-base (n base result)
  (cond ((< n base) (cons n result))
        (t (convert-base (floor n base) base (cons (rem n base) result)))) )

(defun convert (n base)
  (labels ((convert-base (n result)
             (cond ((< n base) (cons n result))
                   (t (convert-base (floor n base) (cons (rem n base) result)))) ))
    (convert-base n '())))

(defun convert (n base)
  (labels ((convert-base (n result)
             (cond ((< n base) (cons n result))
                   (t (multiple-value-bind (f r) (floor n base)
                        (convert-base f (cons r result)))) )))
    (assert (>= n 0) (n) "N must be non-negative: ~D" n)
    (convert-base n '())))

(deftest test-convert ()
  (check
   (equal (convert 0 10) '(0))
   (equal (convert 3 8) '(3))
   (equal (convert 255 10) '(2 5 5))
   (equal (convert 250 10) '(2 5 0))
   (equal (convert 1234 10) '(1 2 3 4))
   (equal (convert 255 2) '(1 1 1 1 1 1 1 1))
   (equal (convert 8 2) '(1 0 0 0))))

(defun rebase (digits in out)
  (cond ((< in 2) nil)
        ((< out 2) nil)
        ((valid-digits-p digits in)
         (convert (evaluate digits in) out))
        (t nil)))

(deftest test-rebase ()
  (check
   (equal '(1) (rebase '(1) 2 10))
   (equal '(5) (rebase '(1 0 1) 2 10))
   (equal '(1 0 1) (rebase '(5) 10 2))
   (equal '(4 2) (rebase '(1 0 1 0 1 0) 2 10))
   (equal '(1 0 1 0 1 0) (rebase '(4 2) 10 2))
   (equal '(2 10) (rebase '(1 1 2 0) 3 16))
   (equal '(1 1 2 0) (rebase '(2 10) 16 3))
   (equal '(6 10 45) (rebase '(3 46 60) 97 73))
   (equal '(0) (rebase '(0) 10 2))
   (equal '(0) (rebase '(0 0 0) 10 2))
   (equal '(4 2) (rebase '(0 6 0) 7 10))
   (not (rebase '(0) 1 10))
   (equal '(0) (rebase 'nil 2 10)) ; See following case?
   (not (rebase 'nil 0 10))
   (not (rebase '(1) -2 10))
   (not (rebase '(1 -1 1 0 1 0) 2 10))
   (not (rebase '(1 2 1 0 1 0) 2 10))
   (not (rebase '(1 0 1 0 1 0) 2 1))
   (not (rebase '(7) 10 0))
   (not (rebase '(1) 2 -7))
   (not (rebase '(1) -2 -7))))

(defpackage :all-your-base-oo (:use :common-lisp :test))

(in-package :all-your-base-oo)

(defclass converter ()
  ((base :reader base :initarg :base)
   (value :reader value)))

(defmethod initialize-instance :after ((c converter) &rest initargs &key digits)
  (declare (ignore initargs))
  (with-slots (base value) c
    (unless (valid-base-p base)
      (error "Invalid base: ~D" base))
    (cond ((null digits) (error "Empty digit list"))
          ((not (valid-digits-p digits base)) (error "Invalid digits for base ~D" base))
          (t (setf value (evaluate digits base)))) ))

(defun valid-base-p (base)
  (>= base 2))

(defun valid-digits-p (digits base)
  (every #'(lambda (digit) (<= 0 digit (1- base))) digits))

(defun evaluate (digits base)
  (reduce #'(lambda (sum d) (+ (* sum base) d)) digits :initial-value 0))

(defgeneric convert (converter base)
  (:documentation "Convert the value of CONVERTER to a representation in BASE."))
(defmethod convert ((c converter) (base integer))
  (assert (valid-base-p base) () "Invalid base: ~D" base)
  (labels ((convert-base (n result)
             (cond ((< n base) (cons n result))
                   (t (multiple-value-bind (f r) (floor n base)
                        (convert-base f (cons r result)))) )))
    (convert-base (value c) '())))

(defun rebase (digits in out)
  (handler-case (let ((converter (make-instance 'converter :base in :digits digits)))
                  (convert converter out))
    (error () nil)
    (:no-error (obj) obj)))

(deftest test-rebase ()
  (check
   (equal '(1) (rebase '(1) 2 10))
   (equal '(5) (rebase '(1 0 1) 2 10))
   (equal '(1 0 1) (rebase '(5) 10 2))
   (equal '(4 2) (rebase '(1 0 1 0 1 0) 2 10))
   (equal '(1 0 1 0 1 0) (rebase '(4 2) 10 2))
   (equal '(2 10) (rebase '(1 1 2 0) 3 16))
   (equal '(1 1 2 0) (rebase '(2 10) 16 3))
   (equal '(6 10 45) (rebase '(3 46 60) 97 73))
   (equal '(0) (rebase '(0) 10 2))
   (equal '(0) (rebase '(0 0 0) 10 2))
   (equal '(4 2) (rebase '(0 6 0) 7 10))
   (not (rebase '(0) 1 10))
;   (equal '(0) (rebase 'nil 2 10)) ; See following case?
   (not (rebase 'nil 0 10))
   (not (rebase '(1) -2 10))
   (not (rebase '(1 -1 1 0 1 0) 2 10))
   (not (rebase '(1 2 1 0 1 0) 2 10))
   (not (rebase '(1 0 1 0 1 0) 2 1))
   (not (rebase '(7) 10 0))
   (not (rebase '(1) 2 -7))
   (not (rebase '(1) -2 -7))))

