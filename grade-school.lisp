;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               grade-school.lisp
;;;;
;;;;   Started:            Sun Dec  8 01:48:48 2024
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
(defpackage :grade-school
  (:use :cl)
  (:export :make-school :add :roster :grade))

(in-package :grade-school)

(defclass school ()
  ((grades :initform (make-hash-table))))

(defun make-school ()
  (make-instance 'school))

(defun add (school name level)
  (with-slots (grades) school
    (loop for grade being each hash-value of grades
          if (get-student grade name) do (return-from add nil))
    (unless (gethash level grades)
      (setf (gethash level grades) (make-instance 'grade :level level)))
    (add-student (gethash level grades) name)))

(defun grade (school level)
  (with-slots (grades) school
    (if (gethash level grades)
        (mapcar #'name (get-students (gethash level grades)))
        nil)))

(defun roster (school)
  (with-slots (grades) school
    (loop for level in (sort (loop for level being each hash-key of grades collect level) #'<)
          nconc (grade school level))))

(defclass grade ()
  ((level :reader level :initarg :level)
   (students :initform (make-array 20 :adjustable t :fill-pointer 0 :element-type 'student))))

(defun get-student (grade name)
  (with-slots (students) grade
    (find name students :key #'name :test #'string-equal)))

(defun add-student (grade name)
  (cond ((get-student grade name) nil)
        (t (with-slots (students) grade
             (vector-push-extend (make-instance 'student :name name) students)
             (setf students (sort students #'string-lessp :key #'name)))) ))

(defun get-students (grade)
  (with-slots (students) grade
    (coerce students 'list)))

(defclass student ()
  ((name :reader name :initarg :name)))
