;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               armstrong.lisp
;;;;
;;;;   Started:            Sun Jun 12 16:09:49 2022
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

(defpackage :armstrong (:use :common-lisp :lang :test))

(in-package :armstrong)

;;;
;;;    See all-your-base.lisp for general split (CONVERT)
;;;    
(defun digits (n)
  (labels ((split (n result)
             (if (< n 10)
                 (cons n result)
                 (multiple-value-bind (q r) (floor n 10)
                   (split q (cons r result)))) ))
    (split n '())))

;;;
;;;    These 3 came from Zhirkov ch. 2 library (print_uint)
;;;    
(defun digit-chars (n)
  (cond ((zerop n) #\0)
        (t (reverse (loop for m = n then (truncate m 10)
                          until (zerop m)
                          collect (digit-char (rem m 10)))) )))

(defun digit-chars (n)
  (do ((m n)
       (result '()))
      (nil)
    (push (digit-char (rem m 10)) result)
    (setf m (truncate m 10))
    (when (zerop m) (return result))))

(defun digit-chars (n)
  (labels ((collect-a (n result)
            (collect-b (truncate n 10) (cons (digit-char (rem n 10)) result)))
           (collect-b (n result)
             (cond ((zerop n) result)
                   (t (collect-a n result)))) )
    (collect-a n '())))

(deftest test-digits ()
  (check
   (equal (digits 0) '(0))
   (equal (digits 1) '(1))
   (equal (digits 10) '(1 0))
   (equal (digits 100) '(1 0 0))
   (equal (digits 23) '(2 3)))) 
   
;; (defun armstrongp (n)
;;   (let* ((digits (digits n))
;;          (count (length digits)))
;;     (= n (reduce #'+ (mapcar #'(lambda (digit) (expt digit count)) digits)))) )

(defun armstrongp (n)
  (let* ((digits (digits n))
         (count (length digits)))
    (= n (reduce #'(lambda (sum digit) (+ sum (expt digit count))) digits :initial-value 0))))

;;;
;;;    https://en.wikipedia.org/wiki/Narcissistic_number#Definition
;;;    
(defun armstrong-number-p (number)
  (assert (typep number '(integer 0)) () "~A must be a natural number." number)
  (loop with k = (1+ (floor (log number 10)))
        for i from 0 below k
        for pow = (expt 10 i)
        for d = (/ (- (mod number (* 10 pow)) (mod number pow)) pow)
        summing (expt d k) into total
        finally (return (= total number))))

(deftest test-armstrongp ()
  (check
    (armstrongp 0)
    (armstrongp 5)
    (not (armstrongp 10))
    (armstrongp 153)
    (not (armstrongp 100))
    (armstrongp 9474)
    (not (armstrongp 9475))
    (armstrongp 9926315)
    (not (armstrongp 9926314))
    (armstrongp 21897142587612075)
    (armstrongp 186709961001538790100634132976990)
    (armstrongp 115132219018763992565095597973971522401))) ; Largest decimal Armstrong #

;; https://web.archive.org/web/20171228054132/https://everything2.net/index.pl?node_id=1407017&displaytype=printable&lastnode_id=1407017
