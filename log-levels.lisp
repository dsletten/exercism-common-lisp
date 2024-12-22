;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               log-levels.lisp
;;;;
;;;;   Started:            Fri Jun 17 14:24:17 2022
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

(defpackage :log-levels (:use :common-lisp :lang :test))

(in-package :log-levels)

;; (defpackage :log-levels
;;   (:use :cl)
;;   (:export :log-message :log-severity :log-format))

;; (in-package :log-levels)

;; (defconstant severity-responses '((info :everything-ok)
;;                                   (warn :getting-worried)
;;                                   (ohno :run-for-cover)))

;; (defun lookup-level (level)
;;   (assoc level severity-responses :key #'symbol-name :test #'string-equal))

;; (defun valid-message-p (msg)
;;   (let ((start (position #\[ msg)))
;;     (if (zerop start)
;;         (let ((end (position #\] msg :start start)))
;;           (if end
;;             (let ((level (subseq msg (1+ start) end)))
;;               (lookup-level level))
;;             nil))
;;         nil)))

;; (defun log-message (log-string)
;;   (if (valid-message-p log-string)
;;       (let ((start (position #\space log-string)))
;;         (if start
;;             (subseq log-string (1+ start))
;;             nil))
;;       nil))

;; (defun log-level (log-string)
;;   (let ((entry (valid-message-p log-string)))
;;     (if entry
;;         (first entry)
;;         nil)))

;; (defun log-severity (log-string)
;;   (let ((level (log-level log-string)))
;;     (if level
;;         (second (lookup-level (symbol-name level)))
;;         nil)))

;; (defun log-format (log-string)
;;   (let ((msg (log-message log-string)))
;;     (ecase (log-severity log-string)
;;       (:everything-ok (string-downcase msg))
;;       (:getting-worried (string-capitalize msg))
;;       (:run-for-cover (string-upcase msg)))) )

(defconstant severity-responses `((info :everything-ok ,#'string-downcase)
                                  (warn :getting-worried ,#'string-capitalize)
                                  (ohno :run-for-cover ,#'string-upcase)))

(defun lookup-level (level)
  (assoc level severity-responses :key #'symbol-name :test #'string-equal))

(defun valid-message-p (msg)
  (let ((start (position #\[ msg)))
    (if (zerop start)
        (let ((end (position #\] msg :start start)))
          (if end
            (let ((level (subseq msg (1+ start) end)))
              (lookup-level level))
            nil))
        nil)))

(defun log-message (log-string)
  (if (valid-message-p log-string)
      (let ((start (position #\space log-string)))
        (if start
            (subseq log-string (1+ start))
            nil))
      nil))

(defun log-level (log-string)
  (let ((entry (valid-message-p log-string)))
    (if entry
        (first entry)
        nil)))

(defun log-severity (log-string)
  (let ((level (log-level log-string)))
    (if level
        (second (lookup-level (symbol-name level)))
        nil)))

(defun log-format (log-string)
  (let ((entry (valid-message-p log-string)))
    (if entry
        (let ((msg (log-message log-string))
              (log-function (third entry)))
          (funcall log-function msg)))) )

(defpackage :log-levels-clos (:use :common-lisp :lang :test))

(in-package :log-levels-clos)

(defclass message ()
  ((level :reader level :initarg :level)
   (content :reader content :initarg :content)
   (severity :reader severity :initarg :severity)))

(defun parse-message (log-string)
  (multiple-value-bind (level-entry content) (valid-message-p log-string)
    (destructuring-bind (level severity) level-entry
      (make-instance 'message :level level :content content :severity severity))))

(defgeneric severity-format (severity content)
  (:documentation "Format CONTENT appropriately based on SEVERITY."))
(defmethod severity-format ((severity (eql :everything-ok)) (content string))
  (declare (ignore severity))
  (string-downcase content))
(defmethod severity-format ((severity (eql :getting-worried)) (content string))
  (declare (ignore severity))
  (string-capitalize content))
(defmethod severity-format ((severity (eql :run-for-cover)) (content string))
  (declare (ignore severity))
  (string-upcase content))

(defmethod print-object ((msg message) stream)
  (with-slots (content severity) msg
    (format stream "~A" (severity-format severity content))))

(defconstant level-severity '(("INFO" :everything-ok)
                              ("WARN" :getting-worried)
                              ("OHNO" :run-for-cover)))

(defun lookup-level (level)
  (assoc level level-severity :test #'string-equal))

(defun valid-message-p (msg)
  (let ((start (position #\[ msg)))
    (if (zerop start)
        (let ((end (position #\] msg :start start)))
          (if end
            (let ((level (subseq msg (1+ start) end)))
              (values (lookup-level (string-upcase level))
                      (string-trim " " (subseq msg (+ end 2)))) )
            nil))
        nil)))

(defun log-message (log-string)
  (content (parse-message log-string)))

(defun log-severity (log-string)
  (severity (parse-message log-string)))

(defun log-format (log-string)
  (write-to-string (parse-message log-string)))
