;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               food-chain.lisp
;;;;
;;;;   Started:            Tue Jul 26 23:11:54 2022
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

(defpackage :food-chain (:use :common-lisp :lang :test))

(in-package :food-chain)

(defun recite (start end)
  (with-output-to-string (s)
    (labels ((i-know (animal)
               (format s "I know an old lady who swallowed a ~A.~%" animal))
             (swallowed (animal reason)
               (format s "She swallowed the ~A to catch the ~A.~%" animal reason))
             (comment (c &key terminal)
               (if terminal
                   (format s "~A" c)
                   (format s "~A~%" c)))
             (end-verse ()
               (format s "~2%"))
             (horse1 (count)
               (unless (= count 1)
                 (cow1 (1- count))
                 (end-verse))
               (i-know "horse")
               (comment "She's dead, of course!" :terminal t))
             (cow1 (count)
               (unless (= count 1)
                 (goat1 (1- count))
                 (end-verse))
               (i-know "cow")
               (comment "I don't know how she swallowed a cow!")
               (cow0))
             (cow0 ()
               (swallowed "cow" "goat")
               (goat0))
             (goat1 (count)
               (unless (= count 1)
                 (dog1 (1- count))
                 (end-verse))
               (i-know "goat")
               (comment "Just opened her throat and swallowed a goat!")
               (goat0))
             (goat0 ()
               (swallowed "goat" "dog")
               (dog0))
             (dog1 (count)
               (unless (= count 1)
                 (cat1 (1- count))
                 (end-verse))
               (i-know "dog")
               (comment "What a hog, to swallow a dog!")
               (dog0))
             (dog0 ()
               (swallowed "dog" "cat")
               (cat0))
             (cat1 (count)
               (unless (= count 1)
                 (bird1 (1- count))
                 (end-verse))
               (i-know "cat")
               (comment "Imagine that, to swallow a cat!")
               (cat0))
             (cat0 ()
               (swallowed "cat" "bird")
               (bird0))
             (bird1 (count)
               (unless (= count 1)
                 (spider1 (1- count))
                 (end-verse))
               (i-know "bird")
               (comment "How absurd to swallow a bird!")
               (bird0))
             (bird0 ()
               (swallowed "bird" "spider that wriggled and jiggled and tickled inside her")
               (spider0))
             (spider1 (count)
               (unless (= count 1)
                 (fly1)
                 (end-verse))
               (i-know "spider")
               (comment "It wriggled and jiggled and tickled inside her.")
               (spider0))
             (spider0 ()
               (swallowed "spider" "fly")
               (fly0))
             (fly1 ()
               (i-know "fly")
               (fly0))
             (fly0 ()
               (comment "I don't know why she swallowed the fly. Perhaps she'll die." :terminal t)))
      (let ((count (1+ (- end start))))
        (ecase end
          (8 (horse1 count))
          (7 (cow1 count))
          (6 (goat1 count))
          (5 (dog1 count))
          (4 (cat1 count))
          (3 (bird1 count))
          (2 (spider1 count))
          (1 (fly1)))) )))
             
(defpackage :food-chain-clos (:use :common-lisp :lang :test))

(in-package :food-chain-clos)

(defclass stanza ()
  ((animal :reader animal :initarg :animal)
   (comment :initarg :comment)
   (alternative :reader alternative :initarg :alternative :initform nil)
   (followup :reader followup :initarg :followup :initform nil)))

(defgeneric comment (stanza)
  (:documentation "Extract the comment of a stanza."))
(defmethod comment ((s stanza))
  (with-slots (comment) s
    (if (null (followup s))
        comment
        (format nil "~A~%" comment))))

(defmacro defstanza (animal comment &optional followup alternative)
  `(make-instance 'stanza
                  :animal ',animal
                  :comment ,comment 
                  ,@(if followup `(:followup ,followup) '())
                  ,@(if alternative `(:alternative ,alternative) '())))

(defvar *stanzas* (let ((h (make-hash-table))
                        (stanzas `((:fly ,(defstanza "fly" "I don't know why she swallowed the fly. Perhaps she'll die."))
                                   (:spider ,(defstanza "spider" "It wriggled and jiggled and tickled inside her." :fly))
                                   (:bird ,(defstanza "bird" "How absurd to swallow a bird!" :spider "spider that wriggled and jiggled and tickled inside her"))
                                   (:cat ,(defstanza "cat" "Imagine that, to swallow a cat!" :bird))
                                   (:dog ,(defstanza "dog" "What a hog, to swallow a dog!" :cat))
                                   (:goat ,(defstanza "goat" "Just opened her throat and swallowed a goat!" :dog))
                                   (:cow ,(defstanza "cow" "I don't know how she swallowed a cow!" :goat))
                                   (:horse ,(defstanza "horse" "She's dead, of course!")))) )
                    (loop for (animal stanza) in stanzas
                          do (setf (gethash animal h) stanza)
                          finally (return h))))

(defun recite (start end)
  (with-output-to-string (s)
    (labels ((i-know (animal)
               (format s "I know an old lady who swallowed a ~A.~%" animal))
             (swallowed (animal reason)
               (format s "She swallowed the ~A to catch the ~A.~%" animal reason))
             (recite-stanza (animal)
               (let ((stanza (gethash animal *stanzas*)))
                 (i-know (animal stanza))
                 (format s (comment stanza))
                 (let ((followup-stanza (gethash (followup stanza) *stanzas*)))
                   (when followup-stanza
                     (recite-followup stanza followup-stanza)))) )
             (recite-followup (old new)
               (swallowed (animal old) (or (alternative old) (animal new)))
               (let ((followup (followup new)))
                 (if followup
                     (recite-followup new (gethash followup *stanzas*))
                     (format s (comment new)))) ))
      (loop for i from 0
            for animal in (subseq '(:fly :spider :bird :cat :dog :goat :cow :horse) (1- start) end)
            unless (zerop i)
             do (format s "~2%")
            end
            do (recite-stanza animal)))) )

;;;
;;;    Second CLOS take inspired by mentoring thelmalu
;;;    
(defpackage :food-chain-clos2 (:use :common-lisp :lang :test))

(in-package :food-chain-clos2)

(defclass stanza ()
  ((animal :reader animal :initarg :animal)
   (comment :initarg :comment)
   (alternative :reader alternative :initarg :alternative :initform nil)
   (followup :reader followup :initarg :followup :initform nil)))

(defgeneric comment (stanza)
  (:documentation "Extract the comment of a stanza."))
(defmethod comment ((s stanza))
  (with-slots (comment) s
    (if (null (followup s))
        comment
        (format nil "~A~%" comment))))

(defmacro defstanza (animal comment &optional followup alternative)
  (let ((comment (interpolate-comment animal comment)))
    `(make-instance 'stanza
                    :animal ',animal
                    :comment ,comment 
                    ,@(if followup `(:followup ,followup) '())
                    ,@(if alternative `(:alternative ,alternative) '()))) )

(defconstant token "*ANIMAL*")

(defun interpolate-comment (animal comment)
  (string-substitute animal token comment :count 1))

(defun string-substitute (new old string &key (count nil) (start 0) (end nil) (test #'char=))
  (let ((match (search old string :test test :start2 start :end2 end)))
    (if (or (null match)
            (and (numberp count) (zerop count)))
        string
        (with-output-to-string (result)
          (string-replace new old string match result :count count :test test :end end)))) )

(defun string-replace (new old source match result-stream &key count test end)
  (labels ((next (count)
             (if (null count)
                 count
                 (1- count)))
           (completed (count)
             (if (null count)
                 nil
                 (zerop count))))
    (do* ((length (length old))
          (count count (next count))
          (index 0 (+ match length))
          (match match (search old source :start2 index :test test :end2 end)))
         ((or (completed count) (null match)) (write-string (subseq source index) result-stream))
      (write-string (subseq source index match) result-stream)
      (write-string new result-stream))))
  

(defvar *stanzas* (let ((h (make-hash-table))
                        (stanzas `((:fly ,(defstanza "fly" "I don't know why she swallowed the *ANIMAL*. Perhaps she'll die."))
                                   (:spider ,(defstanza "spider" "It wriggled and jiggled and tickled inside her." :fly))
                                   (:bird ,(defstanza "bird" "How absurd to swallow a *ANIMAL*!" :spider "spider that wriggled and jiggled and tickled inside her"))
                                   (:cat ,(defstanza "cat" "Imagine that, to swallow a *ANIMAL*!" :bird))
                                   (:dog ,(defstanza "dog" "What a hog, to swallow a *ANIMAL*!" :cat))
                                   (:goat ,(defstanza "goat" "Just opened her throat and swallowed a *ANIMAL*!" :dog))
                                   (:cow ,(defstanza "cow" "I don't know how she swallowed a *ANIMAL*!" :goat))
                                   (:horse ,(defstanza "horse" "She's dead, of course!")))) )
                    (loop for (animal stanza) in stanzas
                          do (setf (gethash animal h) stanza)
                          finally (return h))))

(defun recite (start end)
  (with-output-to-string (s)
    (labels ((i-know (animal)
               (format s "I know an old lady who swallowed a ~A.~%" animal))
             (swallowed (animal reason)
               (format s "She swallowed the ~A to catch the ~A.~%" animal reason))
             (recite-stanza (animal)
               (let ((stanza (gethash animal *stanzas*)))
                 (i-know (animal stanza))
                 (format s (comment stanza))
                 (let ((followup-stanza (gethash (followup stanza) *stanzas*)))
                   (when followup-stanza
                     (recite-followup stanza followup-stanza)))) )
             (recite-followup (old new)
               (swallowed (animal old) (or (alternative old) (animal new)))
               (let ((followup (followup new)))
                 (if followup
                     (recite-followup new (gethash followup *stanzas*))
                     (format s (comment new)))) ))
      (loop for i from 0
            for animal in (subseq '(:fly :spider :bird :cat :dog :goat :cow :horse) (1- start) end)
            unless (zerop i)
             do (format s "~2%")
            end
            do (recite-stanza animal)))) )
