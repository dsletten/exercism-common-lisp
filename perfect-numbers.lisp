;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               perfect-numbers.lisp
;;;;
;;;;   Started:            Thu Jul  7 03:15:00 2022
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
;;;;   ~/lisp/books/Hall/ch02/perfect.lisp
;;;;   33550336 is perfect
;;;;   33550336 = 4096 * 8191 = (* (expt 2 12) (1- (expt 2 13)))
;;;;
;;;;
(defpackage :perfect-numbers (:use :common-lisp))

(in-package :perfect-numbers)

;; (defun classify (n)
;;   (labels ((dividesp (d n) (zerop (mod n d)))
;;            ;; (aliquot-factors (n)
;;            ;;   (remove-if-not #'(lambda (d) (dividesp d n))
;;            ;;                  (loop for i from 1 upto (/ n 2) collect i)))
;;            (aliquot-factors (n)
;;              (remove n (loop for i from 1 to (sqrt n) ; Improved from below
;;                              for (q r) = (multiple-value-list (floor n i))
;;                              when (zerop r) collect i and
;;                              unless (= i q) collect q)))
;;            (examine (n sum)
;;              (cond ((< sum n) "deficient")
;;                ((= sum n) "perfect")
;;                (t "abundant"))))
;;     (cond ((plusp n) (examine n (reduce #'+ (aliquot-factors n))))
;;           (t nil))))

(defun classify (n)
  (labels ((aliquot-factors (n)
             (remove n (loop for i from 1 to (sqrt n)
                             for (q r) = (multiple-value-list (floor n i))
                             when (zerop r) collect i and
                             unless (= i q) collect q)))
           (examine (n sum)
             (cond ((< sum n) "deficient")
                   ((= sum n) "perfect")
                   (t "abundant"))))
    (cond ((plusp n) (examine n (reduce #'+ (aliquot-factors n))))
          (t nil))))

;; ~/lisp/books/Hall/ch02/perfect.lisp
(defun perfectp (n)
  (do ((sum 0 (if (zerop (mod n i)) (+ sum i) sum))
       (i 1 (1+ i)))
      ((> i (truncate n 2)) (= sum n))))

;;;
;;;    https://exercism.org/tracks/common-lisp/exercises/perfect-numbers/solutions/luyiranl2010
;;;    
(defun factors-of (n)
  (loop for i from 1 to (sqrt n)
        for j = (/ n i)
        when (zerop (mod n i))
          collecting i and
        when (/= i j)
          collecting j))   


(defun factors-of (n)
  (loop for i from 1 to (sqrt n)
        for j = (/ n i)
        when (zerop (mod n i)) collect i and ; <-- AND is very important!
        unless (= i j) collect j))   

(defun factors-of* (n)
  (loop for i from 1 to (sqrt n)
        for (q r) = (multiple-value-list (floor n i))
        when (zerop r) collect i and ; <-- AND is very important!
        unless (= i q) collect q))   


;; * (time (classify 8589869056))
;; Evaluation took:
;;   0.024 seconds of real time
;;   0.024118 seconds of total run time (0.024118 user, 0.000000 system)
;;   100.00% CPU
;;   76,861,980 processor cycles
;;   2,981,872 bytes consed
  
;; "perfect"
;; * (time (classify 137438691328))
;; Evaluation took:
;;   0.049 seconds of real time
;;   0.048862 seconds of total run time (0.048862 user, 0.000000 system)
;;   100.00% CPU
;;   155,727,459 processor cycles
;;   11,862,000 bytes consed
  
;; "perfect"
;; * (time (classify 2305843008139952128))
;; Evaluation took:
;;   268.260 seconds of real time
;;   268.194085 seconds of total run time (268.066098 user, 0.127987 system)
;;   [ Real times consist of 0.596 seconds GC time, and 267.664 seconds non-GC time. ]
;;   [ Run times consist of 0.563 seconds GC time, and 267.632 seconds non-GC time. ]
;;   99.98% CPU
;;   854,969,307,321 processor cycles
;;   48,591,980,464 bytes consed
  
;; "perfect"


;;;
;;;    Using FACTORS-OF* algorithm!!!!
;;;    
;; * (time (classify 8589869056))
;; Evaluation took:
;;   0.003 seconds of real time
;;   0.002730 seconds of total run time (0.002730 user, 0.000000 system)
;;   100.00% CPU
;;   8,697,847 processor cycles
;;   0 bytes consed
  
;; "perfect"
;; * (time (classify 137438691328))
;; Evaluation took:
;;   0.008 seconds of real time
;;   0.007069 seconds of total run time (0.007069 user, 0.000000 system)
;;   87.50% CPU
;;   22,527,574 processor cycles
;;   0 bytes consed
  
;; "perfect"
;; * (time (classify 2305843008139952128))
;; Evaluation took:
;;   8.831 seconds of real time
;;   8.831123 seconds of total run time (8.831123 user, 0.000000 system)
;;   100.00% CPU
;;   28,148,154,025 processor cycles
;;   0 bytes consed
  
;; "perfect"


;; 2 6
;; 3 28
;; 5 496
;; 7 8128
;; 13 33550336
;; 17 8589869056
;; 19 137438691328
;; 31 2305843008139952128
;; 61 2658455991569831744654692615953842176
;; 89 191561942608236107294793378084303638130997321548169216
;; 107 13164036458569648337239753460458722910223472318386943117783728128
;; 127 14474011154664524427946373126085988481573677491474835889066354349131199152128
;; 521 23562723457267347065789548996709904988477547858392600710143027597506337283178622239730365539602600561360255566462503270175052892578043215543382498428777152427010394496918664028644534128033831439790236838624033171435922356643219703101720713163527487298747400647801939587165936401087419375649057918549492160555646976
;; 607 141053783706712069063207958086063189881486743514715667838838675999954867742652380114104193329037690251561950568709829327164087724366370087116731268159313652487450652439805877296207297446723295166658228846926807786652870188920867879451478364569313922060370695064736073572378695176473055266826253284886383715072974324463835300053138429460296575143368065570759537328128
;; 1279 54162526284365847412654465374391316140856490539031695784603920818387206994158534859198999921056719921919057390080263646159280013827605439746262788903057303445505827028395139475207769044924431494861729435113126280837904930462740681717960465867348720992572190569465545299629919823431031092624244463547789635441481391719816441605586788092147886677321398756661624714551726964302217554281784254817319611951659855553573937788923405146222324506715979193757372820860878214322052227584537552897476256179395176624426314480313446935085203657584798247536021172880403783048602873621259313789994900336673941503747224966984028240806042108690077670395259231894666273615212775603535764707952250173858305171028603021234896647851363949928904973292145107505979911456221519899345764984291328
       
