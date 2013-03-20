;;;; cl-flexoplan.lisp

(in-package #:cl-flexoplan)

(cl-interpol:enable-interpol-syntax)

(defparameter last-id -1)
(defparameter goals (make-hash-table :test #'equal))

;;; Now we implement simple in-memory storage of these "goals" (know as "tickets" elsewhere).
(defclass goal ()
  ((id :initform (incf last-id) :initarg :id :accessor goal-id)
   (title :initform (error "Title should be specified") :initarg :title :accessor goal-title)
   (description :initform nil :initarg :description :accessor goal-description)
   (subgoals :initform nil :initarg :subgoals :accessor goal-subgoals)
   (status :initform "open" :initarg :status :accessor goal-status)
   (priority :initform  5 :initarg :priority :accessor goal-priority)))

(defun drop-all-goals ()
  (setf last-id -1)
  (clrhash goals))

(defun add-goal (title &optional append-to)
  "For now very naive version of ADD-GOAL."
  (let ((goal (make-instance 'goal :title title)))
    (setf (gethash (goal-id goal) goals) goal)
    (if append-to
	(push goal (goal-subgoals (gethash append-to goals))))
    (goal-id goal)))

(defparameter indent 0)

(defun display-goal (goal)
  (format nil "~a* ~a~a~%~{~a~}"
	  (make-string indent :initial-element #\space)
	  (if (not (equal (goal-status goal) "open"))
	      #?"($((goal-status goal))) "
	      "")
	  (goal-title goal)
	  (let ((indent (+ indent 2)))
	    (mapcar #'display-goal (reverse (goal-subgoals goal))))))
	      

#+nil
(progn
  (let ((master-id (add-goal "Сделать дело")))
    (add-goal "Сделать раз" master-id)
    (add-goal "Сделать два" master-id)
    (add-goal "Сделать три" master-id)))

(defvar *goal-template-rules* (make-hash-table))

(defmacro with-goal-template-rules (&body body)
  `(let ((esrap::*rules* *goal-template-rules*))
     ,@body))

(defun goal-template-parse (symbol text)
  (with-goal-template-rules
    (esrap:parse symbol text)))

(defmacro define-rule (symbol expression &body options)
  `(with-goal-template-rules
     (defrule ,symbol ,expression ,@options)))

(defun lispify-string (str)
  (coerce (iter (for ch in-string str)
                (for prev-char previous ch)
                (when (and (upper-case-p ch) prev-char (or (digit-char-p prev-char)
                                                           (lower-case-p prev-char)))
                  (collect #\-))
                (collect (char-upcase ch)))
          'string))

(defun lispify-name (str)
  (intern (lispify-string str) :keyword))

(defun not-equal (obj1 obj2)
  (not (equal obj1 obj2)))

(define-rule whitespace (+ (or #\space #\tab #\newline))
  (:constant " "))

(define-rule n-spaces (* #\space)
  (:lambda (lst)
    (length lst)))

(define-rule alpha-char (character-ranges (#\a #\z) (#\A #\Z)))
(define-rule alnum-char (or alpha-char (character-ranges (#\0 #\9))))

(define-rule simple-name (and alpha-char (* (or alnum-char #\_)))
  (:text t))

(define-rule status (and "(" simple-name ")")
  (:destructure (start text end)
		(declare (ignore start end))
		(text text)))

(define-rule goal-title (* (and (not #\newline)))
  (:text t))

(define-rule goal (and n-spaces "*" (? whitespace) (? status) (? whitespace) goal-title)
  (:destructure (n-spaces ast wh1 status wh2 title)
		(declare (ignore ast wh1 wh2))
		`(,n-spaces ,status ,title)))

(define-rule goal-chart (and goal (* (and #\newline goal)))
  (:destructure (first-goal rest-goals)
		`(,first-goal ,@(mapcar (lambda (x)
					  (cadr x))
					rest-goals))))

(defparameter new-plan
  "* project
  * goal1
  * goal2
  * goal3
*project2
  * goal11
  * goal12
  * goal13")
