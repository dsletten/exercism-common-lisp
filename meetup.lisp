#!/usr/bin/sbcl --script
;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   If you give someone Fortran, he has Fortran. If you give someone Lisp, he has any language he pleases.
;;;;   -- Guy Steele
;;;;
;;;;   Name:               meetup.lisp
;;;;
;;;;   Started:            Sat Dec  7 18:57:54 2024
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
(defpackage :meetup
  (:use :cl)
  (:export :meetup))

(in-package :meetup)

;(meetup 5 2013 :monday :teenth) => (2013 5 13) 

(defvar *weekdays* '(:monday :tuesday :wednesday :thursday :friday :saturday :sunday))
(defvar *weeks* '(:first :second :third :fourth))

(defun day-of-week (day month year)
  (nth-value 6 (decode-universal-time (encode-universal-time 0 0 0 day month year))))

(defun leap-year-p (year)
  (cond ((zerop (mod year 400)) t)
        ((zerop (mod year 100)) nil)
        (t (zerop (mod year 4)))) )

(defun month-length (month year)
  (ccase month
    ((4 6 9 11) 30)
    (2 (if (leap-year-p year) 29 28))
    ((1 3 5 7 8 10 12) 31)))

(defun calculate-day-of-month (month year dow week)        
  (let ((first (day-of-week 1 month year)))
    (1+ (+ (mod (+ (- dow first) 7) 7) (* 7 week)))) )

(defun day-of-month (month year weekday schedule)
  (let ((dow (position weekday *weekdays*)))
    (case schedule
      (:teenth
       (loop for week from 1
             for day-of-month = (calculate-day-of-month month year dow week)
             until (>= day-of-month 13)
             finally (return day-of-month)))
      (:last
       (loop with month-length = (month-length month year)
             for week from 3
             for day-of-month = (calculate-day-of-month month year dow week)
             until (> day-of-month (- month-length 7))
             finally (return day-of-month)))
      (otherwise
       (calculate-day-of-month month year dow (position schedule *weeks*)))) ))

(defun meetup (month year weekday schedule)
  "Returns a date in the format (y m d) for a given meetup date."
  (list year month (day-of-month month year weekday schedule)))

;; ncal -Mb 2024
;;                             2024
;;       January               February               March          
;; Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  
;;  1  2  3  4  5  6  7            1  2  3  4               1  2  3  
;;  8  9 10 11 12 13 14   5  6  7  8  9 10 11   4  5  6  7  8  9 10  
;; 15 16 17 18 19 20 21  12 13 14 15 16 17 18  11 12 13 14 15 16 17  
;; 22 23 24 25 26 27 28  19 20 21 22 23 24 25  18 19 20 21 22 23 24  
;; 29 30 31              26 27 28 29           25 26 27 28 29 30 31  
                                                                  

;;        April                  May                   June          
;; Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  
;;  1  2  3  4  5  6  7         1  2  3  4  5                  1  2  
;;  8  9 10 11 12 13 14   6  7  8  9 10 11 12   3  4  5  6  7  8  9  
;; 15 16 17 18 19 20 21  13 14 15 16 17 18 19  10 11 12 13 14 15 16  
;; 22 23 24 25 26 27 28  20 21 22 23 24 25 26  17 18 19 20 21 22 23  
;; 29 30                 27 28 29 30 31        24 25 26 27 28 29 30  
                                                                  

;;         July                 August              September        
;; Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  
;;  1  2  3  4  5  6  7            1  2  3  4                     1  
;;  8  9 10 11 12 13 14   5  6  7  8  9 10 11   2  3  4  5  6  7  8  
;; 15 16 17 18 19 20 21  12 13 14 15 16 17 18   9 10 11 12 13 14 15  
;; 22 23 24 25 26 27 28  19 20 21 22 23 24 25  16 17 18 19 20 21 22  
;; 29 30 31              26 27 28 29 30 31     23 24 25 26 27 28 29  
;;                                             30                    

;;       October               November              December        
;; Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su  
;;     1  2  3  4  5  6               1  2  3                     1  
;;  7  8  9 10 11 12 13   4  5  6  7  8  9 10   2  3  4  5  6  7  8  
;; 14 15 16 17 18 19 20  11 12 13 14 15 16 17   9 10 11 12 13 14 15  
;; 21 22 23 24 25 26 27  18 19 20 21 22 23 24  16 17 18 19 20 21 22  
;; 28 29 30 31           25 26 27 28 29 30     23 24 25 26 27 28 29  
;;                                             30 31                 

;; cal 2024
;;                             2024
;;       January               February               March          
;; Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  
;;     1  2  3  4  5  6               1  2  3                  1  2  
;;  7  8  9 10 11 12 13   4  5  6  7  8  9 10   3  4  5  6  7  8  9  
;; 14 15 16 17 18 19 20  11 12 13 14 15 16 17  10 11 12 13 14 15 16  
;; 21 22 23 24 25 26 27  18 19 20 21 22 23 24  17 18 19 20 21 22 23  
;; 28 29 30 31           25 26 27 28 29        24 25 26 27 28 29 30  
;;                                             31                    

;;        April                  May                   June          
;; Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  
;;     1  2  3  4  5  6            1  2  3  4                     1  
;;  7  8  9 10 11 12 13   5  6  7  8  9 10 11   2  3  4  5  6  7  8  
;; 14 15 16 17 18 19 20  12 13 14 15 16 17 18   9 10 11 12 13 14 15  
;; 21 22 23 24 25 26 27  19 20 21 22 23 24 25  16 17 18 19 20 21 22  
;; 28 29 30              26 27 28 29 30 31     23 24 25 26 27 28 29  
;;                                             30                    

;;         July                 August              September        
;; Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  
;;     1  2  3  4  5  6               1  2  3   1  2  3  4  5  6  7  
;;  7  8  9 10 11 12 13   4  5  6  7  8  9 10   8  9 10 11 12 13 14  
;; 14 15 16 17 18 19 20  11 12 13 14 15 16 17  15 16 17 18 19 20 21  
;; 21 22 23 24 25 26 27  18 19 20 21 22 23 24  22 23 24 25 26 27 28  
;; 28 29 30 31           25 26 27 28 29 30 31  29 30                 
                                                                  

;;       October               November              December        
;; Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  Su Mo Tu We Th Fr Sa  
;;        1  2  3  4  5                  1  2   1  2  3  4  5  6  7  
;;  6  7  8  9 10 11 12   3  4  5  6  7  8  9   8  9 10 11 12 13 14  
;; 13 14 15 16 17 18 19  10 11 12 13 14 15 16  15 16 17 18 19 20 21  
;; 20 21 22 23 24 25 26  17 18 19 20 21 22 23  22 23 24 25 26 27 28  
;; 27 28 29 30 31        24 25 26 27 28 29 30  29 30 31              
