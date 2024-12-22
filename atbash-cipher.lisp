;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               atbash-cipher.lisp
;;;;
;;;;   Started:            Sat Jul  2 19:20:12 2022
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

(defpackage :atbash-cipher (:use :common-lisp :lang :test))

(in-package :atbash-cipher)

(defconstant plain (loop for i from (char-code #\a) to (char-code #\z)
                         collect (code-char i) into chars
                         finally (return (coerce chars 'string))))
(defconstant cipher (reverse plain))
(defconstant atbash (let ((h (make-hash-table :test #'equalp)))
                         (loop for p across plain
                               for c across cipher
                               do (setf (gethash p h) c)
;                                        (gethash (char-upcase p) h) (char-upcase c))
                               finally (return h))))
(defun encode (plaintext)
  (let ((text (remove-if-not #'alphanumericp plaintext)))
       (with-input-from-string (in text)
         (with-output-to-string (out)
           (loop until (null (peek-char nil in nil))
                 do (translate in out 5))))))

(defun translate (in out n)
  (unless (null (peek-char nil in nil))
    (cond ((zerop n) (write-char #\space out))
          (t (let ((ch (read-char in)))
                (write-char (gethash ch atbash ch) out)
               (translate in out (1- n)))))))


          


        

          
;;;
;;;    mhadam
;;;    
;; (defun opposite-letter (letter)
;;   (code-char (- 219 (char-code letter))))

;; (defun encode (plaintext)
;;   (loop for character across plaintext
;;      when (alpha-char-p character) collect (opposite-letter (char-downcase character)) into output
;;      when (digit-char-p character) collect character into output
;;      finally (return (format nil "~{~^~c~^~c~^~c~^~c~^~c~^ ~}" output))))

;;  97  98  99 100 ... 122
;;   a   b   c   d       z
;;   z   y   x   w       a
;; 122 121 120 119      97
;; 219 219 219 219     219
