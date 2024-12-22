;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Pascal is for building pyramids—imposing, breathtaking, static structures built by armies pushing heavy blocks into place. Lisp is for building organisms...
;;;;   -- Alan Perlis
;;;;
;;;;   Name:               roman-numerals.lisp
;;;;
;;;;   Started:            Fri Jun 17 00:46:52 2022
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

(load "/home/slytobias/lisp/books/Concise/concise.lisp")

(defpackage :roman-numerals (:use :common-lisp :lang :test))

(in-package :roman-numerals)

(defun romanize (number)
  (with-output-to-string (s)
    (labels ((m (n)
               (cond ((>= n 1000)
                      (write-string "M" s)
                      (m (- n 1000)))
                     (t (cm n))))
             (cm (n)
               (cond ((>= n 900)
                      (write-string "CM" s)
                      (xc (- n 900)))
                     (t (d n))))
             (d (n)
               (cond ((>= n 500)
                      (write-string "D" s)
                      (c (- n 500)))
                     (t (cd n))))
             (cd (n)
               (cond ((>= n 400)
                      (write-string "CD" s)
                      (xc (- n 400)))
                     (t (c n))))
             (c (n)
               (cond ((>= n 100)
                      (write-string "C" s)
                      (c (- n 100)))
                     (t (xc n))))
             (xc (n)
               (cond ((>= n 90)
                      (write-string "XC" s)
                      (ix (- n 90)))
                     (t (l n))))
             (l (n)
               (cond ((>= n 50)
                      (write-string "L" s)
                      (x (- n 50)))
                     (t (xl n))))
             (xl (n)
               (cond ((>= n 40)
                      (write-string "XL" s)
                      (ix (- n 40)))
                     (t (x n))))
             (x (n)
               (cond ((>= n 10)
                      (write-string "X" s)
                      (x (- n 10)))
                     (t (ix n))))
             (ix (n)
               (cond ((= n 9)
                      (write-string "IX" s)
                      s)
                     (t (v n))))
             (v (n)
               (cond ((>= n 5)
                      (write-string "V" s)
                      (i (- n 5)))
                     (t (iv n))))
             (iv (n)
               (cond ((= n 4)
                      (write-string "IV" s)
                      s)
                     (t (i n))))
             (i (n)
               (cond ((>= n 1)
                      (write-string "I" s)
                      (i (- n 1)))
                     (t s))))
      (cond ((<= number 0) (error "No way, Jose"))
            ((>= number 4000) (error "Number too large to print in Roman numerals: ~D" number))
            (t (m number)))) ))

(deftest test-romanize ()
  (check
   (every #'(lambda (n) (string= (romanize n) (format nil "~@R" n))) (loop for i from 1 to 3999 collect i))))

;; (defmacro roman-state (stream name threshold above &optional below)
;;   (let ((n (gensym))
;;         (test (if (listp threshold) #'>= #'=))
;;         (limit (if (listp threshold) (first threshold) threshold)))
;;     `(,name (,n)
;;        (cond ((funcall ,test ,n ,limit) (write-string ,(symbol-name name) ,stream) ,(if (null above) nil `(,above (- ,n ,limit))))
;;              (t ,(if (null below) nil `(,below ,n)))) )))

;; (defmacro roman-fsm (states)
;;   (let ((stream (gensym)))
;;     `(with-output-to-string (,stream)
;;        (labels ,(mapcar #'(lambda (state) (list* 'roman-state stream state)) states)))) )

(defmacro roman-fsm (states &body body)
  (let* ((stream (gensym))
         (fsm (loop for (name threshold above below) in states
                    for n = (gensym)
                    for test = (if (listp threshold) '>= '=)
                    for limit = (if (listp threshold) (first threshold) threshold)
                    collect `(,name (,n)
                                (cond ((,test ,n ,limit) (write-string ,(symbol-name name) ,stream) ,(if (null above) nil `(,above (- ,n ,limit))))
                                      (t ,(if (null below) nil `(,below ,n)))) ))))
    `(with-output-to-string (,stream)
       (labels ,fsm
         ,@body))))

(defun romanize* (number)
  (roman-fsm
   ((m (1000) m cm)
    (cm (900) xc d)
    (d (500) c cd)
    (cd (400) xc c)
    (c (100) c xc)
    (xc (90) ix l)
    (l (50) x xl)
    (xl (40) ix x)
    (x (10) x ix)
    (ix 9 nil v)
    (v (5) i iv)
    (iv 4 nil i)
    (i (1) i nil))
   (cond ((<= number 0) (error "No way, Jose"))
         ((>= number 4000) (error "Number too large to print in Roman numerals: ~D" number))
         (t (m number)))) )

(deftest test-romanize* ()
  (check
   (every #'(lambda (n) (string= (romanize* n) (format nil "~@R" n))) (loop for i from 1 to 3999 collect i))))

;; (macroexpand-1 '(roman-fsm
;;    ((m (1000) m cm)
;;     (cm (900) xc d)
;;     (d (500) cd cd)
;;     (cd (400) xc c)
;;     (c (100) c xc)
;;     (xc (90) ix l)
;;     (l (50) x xl)
;;     (xl (40) ix x)
;;     (x (10) x ix)
;;     (ix 9 nil v)
;;     (v (5) i iv)
;;     (iv 4 nil i)
;;     (i (1) i nil))
;;    (cond ((<= number 0) (error "No way, Jose"))
;;          ((>= number 4000) (error "Number too large to print in Roman numerals: ~D" number))
;;          (t (m number)))))
;; (WITH-OUTPUT-TO-STRING (#:G571)
;;   (LABELS ((M (#:G572)
;;              (COND
;;               ((>= #:G572 1000) (WRITE-STRING "M" #:G571) (M (- #:G572 1000)))
;;               (T (CM #:G572))))
;;            (CM (#:G573)
;;              (COND
;;               ((>= #:G573 900) (WRITE-STRING "CM" #:G571) (XC (- #:G573 900)))
;;               (T (D #:G573))))
;;            (D (#:G574)
;;              (COND
;;               ((>= #:G574 500) (WRITE-STRING "D" #:G571) (CD (- #:G574 500)))
;;               (T (CD #:G574))))
;;            (CD (#:G575)
;;              (COND
;;               ((>= #:G575 400) (WRITE-STRING "CD" #:G571) (XC (- #:G575 400)))
;;               (T (C #:G575))))
;;            (C (#:G576)
;;              (COND
;;               ((>= #:G576 100) (WRITE-STRING "C" #:G571) (C (- #:G576 100)))
;;               (T (XC #:G576))))
;;            (XC (#:G577)
;;              (COND
;;               ((>= #:G577 90) (WRITE-STRING "XC" #:G571) (IX (- #:G577 90)))
;;               (T (L #:G577))))
;;            (L (#:G578)
;;              (COND ((>= #:G578 50) (WRITE-STRING "L" #:G571) (X (- #:G578 50)))
;;                    (T (XL #:G578))))
;;            (XL (#:G579)
;;              (COND
;;               ((>= #:G579 40) (WRITE-STRING "XL" #:G571) (IX (- #:G579 40)))
;;               (T (X #:G579))))
;;            (X (#:G580)
;;              (COND ((>= #:G580 10) (WRITE-STRING "X" #:G571) (X (- #:G580 10)))
;;                    (T (IX #:G580))))
;;            (IX (#:G581)
;;              (COND ((= #:G581 9) (WRITE-STRING "IX" #:G571) NIL)
;;                    (T (V #:G581))))
;;            (V (#:G582)
;;              (COND ((>= #:G582 5) (WRITE-STRING "V" #:G571) (I (- #:G582 5)))
;;                    (T (IV #:G582))))
;;            (IV (#:G583)
;;              (COND ((= #:G583 4) (WRITE-STRING "IV" #:G571) NIL)
;;                    (T (I #:G583))))
;;            (I (#:G584)
;;              (COND ((>= #:G584 1) (WRITE-STRING "I" #:G571) (I (- #:G584 1)))
;;                    (T NIL))))
;;     (COND ((<= NUMBER 0) (ERROR "No way, Jose"))
;;           ((>= NUMBER 4000)
;;            (ERROR "Number too large to print in Roman numerals: ~D" NUMBER))
;;           (T (M NUMBER)))))


(defmacro roman-fsm-q (states &body body)
  (let* ((fsm (loop for (name threshold above below) in states
                    for n = (gensym)
                    for q = (gensym)
                    for test = (if (listp threshold) '>= '=)
                    for limit = (if (listp threshold) (first threshold) threshold)
                    collect `(,name (,n ,q)
                                (cond ((,test ,n ,limit) 
                                       ,(if (eq above :end) `(containers:enqueue ,q ,(symbol-name name))
                                            `(,above (- ,n ,limit) (containers:enqueue ,q ,(symbol-name name)))))
                                      (t ,(if (eq below :end) q `(,below ,n ,q)))) ))))
    `(labels ,(cons (let ((n (gensym)))
                      `(:start (,n)
                         (,(first (first states)) ,n (make-instance 'containers:persistent-queue))))
                    fsm)
         ,@body)))

;;;
;;;    :START names the initial state. The code should explicitly tie this to the first
;;;    user-defined state (M here) rather than implicitly relying on the order of the
;;;    states in the macro!
;;;    
(defun romanizeq (number)
  (roman-fsm-q
   ((m (1000) m cm)
    (cm (900) xc d)
    (d (500) c cd)
    (cd (400) xc c)
    (c (100) c xc)
    (xc (90) ix l)
    (l (50) x xl)
    (xl (40) ix x)
    (x (10) x ix)
    (ix 9 :end v)
    (v (5) i iv)
    (iv 4 :end i)
    (i (1) i :end))
   (cond ((<= number 0) (error "No way, Jose"))
         ((>= number 4000) (error "Number too large to print in Roman numerals: ~D" number))
         (t (:start number)))) )

(deftest test-romanizeq ()
  (check
   (every #'(lambda (n) (string= (with-output-to-string (out) (loop for q = (romanizeq n) then (containers:dequeue q) until (containers:emptyp q) do (write-string (containers:front q) out))) (format nil "~@R" n))) (loop for i from 1 to 3999 collect i))))

;;;;
;;;;    http://www.novaroma.org/via_romana/numbers.html
;;;;    
(defpackage :roman-numerals-crazy-javascript (:use :common-lisp :lang :test))

(in-package :roman-numerals-crazy-javascript)

(defclass level ()
  ((i :reader i :initarg :i)
   (v :reader v :initarg :v)
   (x :reader x :initarg :x)))

(defconstant levels (vector (make-instance 'level :i "I" :v "V" :x "X")
                            (make-instance 'level :i "X" :v "L" :x "C")
                            (make-instance 'level :i "C" :v "D" :x "M")))

(defun calc-digit (d l)
  (with-output-to-string (s)
    (cond ((> l 2) (do ((m 1 (1+ m)))
                       ((> m (* d (expt 10 (- l 3)))) )
                     (write-string "M" s)))
          ((< d 4) (loop repeat d
                         do (write-string (i (elt levels l)) s)))
          ((= d 4)
           (write-string (i (elt levels l)) s)
           (write-string (v (elt levels l)) s))
          ((< d 9)
           (write-string (v (elt levels l)) s)
           (loop repeat (- d 5)
                 do (write-string (i (elt levels l)) s)))
          ((= d 9)
           (write-string (i (elt levels l)) s)
           (write-string (x (elt levels l)) s))
          (t nil))))

(defun digits (n)
  (labels ((split (n result)
             (if (< n 10)
                 (cons n result)
                 (multiple-value-bind (q r) (floor n 10)
                   (split q (cons r result)))) ))
    (split n '())))

(defun romanize (n)
  (with-output-to-string (r)
    (let* ((digits (digits n))
           (count (length digits)))
      (loop for digit in digits
            for c below count
            do (write-string (calc-digit digit (- count c 1)) r)))) )

;;;
;;;    Inspired by Shane Laymance's JavaScript solution
;;;    github.com/slaymance/exercism/tree/main/javascript
;;;    
(defpackage :roman-numerals-slaymance (:use :common-lisp :lang :test))

(in-package :roman-numerals-slaymance)

(defconstant roman-values '((1000 m)
                            (900 cm)
                            (500 d)
                            (400 cd)
                            (100 c)
                            (90 xc)
                            (50 l)
                            (40 xl)
                            (10 x)
                            (9 ix)
                            (5 v)
                            (4 iv)
                            (1 i)))

(defun romanize (n)
  (labels ((to-roman (n q)
             (if (zerop n)
                 q
                 (destructuring-bind (value numeral) (assoc n roman-values :test #'>=)
                   (to-roman (- n value) (containers:enqueue q (symbol-name numeral)))) )))
    (with-output-to-string (out)
      (loop for q = (to-roman n (make-instance 'containers:persistent-queue)) then (containers:dequeue q)
            until (containers:emptyp q) 
            do (write-string (containers:front q) out)))) )
    
(deftest test-romanize ()
  (check
   (every #'(lambda (n) (string= (romanize n) (format nil "~@R" n))) (loop for i from 1 to 3999 collect i))))
