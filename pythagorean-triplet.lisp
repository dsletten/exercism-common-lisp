;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               pythagorean-triplet.lisp
;;;;
;;;;   Started:            Wed Jul 20 01:10:17 2022
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
;;;;   https://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples
;;;;   https://en.wikipedia.org/wiki/Pythagorean_triple
;;;;
;;;;
(defpackage :pythagorean-triplet
  (:use :cl)
  (:export :triplets-with-sum))

(in-package :pythagorean-triplet)

;; (defun triplets-with-sum (n)
;;   (let ((triplets '()))
;;     (loop for c from 1 to (floor n 2)
;;           for c² = (* c c)
;;           do (loop for b from 1 below c
;;                    for b² = (* b b)
;; ;                   do (loop for a from (- c b) below (+ c b)
;;                    do (loop for a from (- c b) below b
;; ;                            while (<= (+ a b c) n)
;;                             when (and (= c² (+ (* a a) b²))
;;                                       (= (+ a b c) n))
;;                               do (let ((triplet (sort (list a b c) #'<)))
;;                                    (unless (member triplet triplets :test #'equal)
;; (print triplet)
;;                                      (push triplet triplets)))) ))
;;        triplets))

(defun triplets-with-sum (n)
  (let ((triplets '()))
    (loop for c from 1 to (floor n 2)
          for c² = (* c c)
          do (loop for b from 1 below c
                   for b² = (* b b)
                   for a = (- n b c)
                   when (= c² (+ (* a a) b²))
                     do (let ((triplet (sort (list a b c) #'<)))
                          (unless (member triplet triplets :test #'equal)
                            (push triplet triplets)))) )
       triplets))


        

          
;;;
;;;    https://exercism.org/tracks/common-lisp/exercises/pythagorean-triplet/solutions/PDT89
;;;    
;(defparameter +right-isoceles-perimeter-ratio+ 3.414) ; approx 1 + 1 + 2^(1/2)

;; (defun triplets-with-sum (n)
;;   (loop for a from 1 to (floor n +right-isoceles-perimeter-ratio+) with c and modulo
;;         do (multiple-value-setq (c modulo) (floor (+ (expt a 2) (expt (- n a) 2)) (* 2 (- n a))))
;;         when (zerop modulo)
;;         collect (list a (- n a c) c)))

;;;
;;;    For right triangle with legs a, b: a² + b² = c²
;;;    Limiting case where a = b: a² + a² = 2a² = c²
;;;    Therefore: c = √2a
;;;    
;;;    If a + b + c = n, then for isosceles right triangle: a + a + √2a = n
;;;    or:         n
;;;         a = --------
;;;              2 + √2
;;;
;;;    If a were greater than this, then b would be smaller, simply leading to duplicate triples.
;;;    Thus this is the upper limit on values to check for a.
;;;    
;;;    This is the limit of the ratio of legs a, b: a = b
;;;    a < b => a < n/(2 + √2)
;;;    
(defconstant right-isoceles-perimeter-ratio (+ 1 1 (sqrt 2d0)))

;;;
;;;    Assuming that a + b + c = n and a² + b² = c², then:
;;;    b + c = n - a
;;;    and
;;;    a² + b² + 2bc + c² = c² + 2bc + c²
;;;    a² + (b + c)² = 2bc + 2c²
;;;    a² + (b + c)² = c(2b + 2c)
;;;    
;;;    a² + (b + c)²
;;;    ------------- = c
;;;      2(b + c)
;;;       
;;;    Substituting n - a for b + c:
;;;    a² + (n - a)²
;;;    ------------- = c
;;;      2(n - a)
;;;       
(defun triplets-with-sum* (n)
  (loop for a from 1 to (floor n right-isoceles-perimeter-ratio)
        for (c modulo) = (multiple-value-list (floor (+ (expt a 2) (expt (- n a) 2)) (* 2 (- n a))))
        when (zerop modulo)
        collect (list a (- n a c) c)))

(defun triplets-with-sum* (n)
  (loop for a from 1 to (floor n right-isoceles-perimeter-ratio)
        for b+c = (- n a)
        for (c modulo) = (multiple-value-list (floor (+ (expt a 2) (expt b+c 2)) (* 2 b+c)))
        when (zerop modulo) collect (list a (- b+c c) c)))


        

          
;;;
;;;    https://exercism.org/tracks/common-lisp/exercises/pythagorean-triplet/solutions/leetwinski
;;;    
(defun range (start end-excl)
  (loop for i from start below end-excl collect i))

(defun triplets-with-sum (n)
  (loop for a in (range 1 (/ n 3))
        nconc (loop for b in (range (1+ a) (* 2 (/ n 3)))
                    for c = (- n a b)
                    when (= (+ (* a a) (* b b)) (* c c))
                      collect (list a b c))))
