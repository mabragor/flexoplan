;;;; cl-flexoplan.lisp

(in-package #:cl-flexoplan)

(cl-interpol:enable-interpol-syntax)

(defparameter *last-id* -1)
(defparameter *goals* (make-hash-table :test #'equal))

;;; Now we implement simple in-memory storage of these "goals" (know as "tickets" elsewhere).
(defclass goal ()
  ((id :initform (incf last-id) :initarg :id :accessor goal-id)
   (title :initform (error "Title should be specified") :initarg :title :accessor goal-title)
   (description :initform nil :initarg :description :accessor goal-description)
   ;; (subgoals :initform nil :initarg :subgoals :accessor goal-subgoals)
   (status :initform "open" :initarg :status :accessor goal-status)
   (priority :initform  5 :initarg :priority :accessor goal-priority)))

(defun drop-all-goals ()
  (setf last-id -1)
  (clrhash *goals*))

(defun add-goal (title) ; &optional append-to)
  "For now very naive version of ADD-GOAL."
  (let ((goal (make-instance 'goal :title title)))
    (setf (gethash (goal-id goal) *goals*) goal)
    ;; (if append-to
    ;; 	(push goal (goal-subgoals (gethash append-to *goals*))))
    (goal-id goal)))

(defparameter indent 0)

(defun display-goal (goal)
  (format nil "~a* ~a~a~%" ; ~{~a~}"
	  (make-string indent :initial-element #\space)
	  (if (not (equal (goal-status goal) "open"))
	      #?"($((goal-status goal))) "
	      "")
	  (goal-title goal)))
	  ;; (let ((indent (+ indent 2)))
	  ;;   (mapcar #'display-goal (reverse (goal-subgoals goal))))))

(defparameter displayed-goals nil
  "Map between lines and ids of currently displayed goals.")
(defparameter cur-line 0
  "Line, which we are currently trying to display.")

(defun display-goals-flat (&optional
			     (predicate #'identity))
  (with-output-to-string (stream)
    (iter (for (key val) in-hashtable *goals*)
	  (if (not (funcall predicate val))
	      (next-iteration))
	  (push `(,cur-line . ,key) displayed-goals)
	  (incf cur-line)
	  (format stream (display-goal val)))))

(defun make-start-test-goal-set (&optional (n 3))
  (drop-all-goals)
  (add-goal "Make a Deed")
  (iter (for i from 1 to n)
	(add-goal (format nil "Do punkt #~a" i))))

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

(define-rule goal-chart (and goal (* (and #\newline goal)) (? #\newline))
  (:destructure (first-goal rest-goals newline)
		(declare (ignore newline))
		`(,first-goal ,@(mapcar (lambda (x)
					  (cadr x))
					rest-goals))))

(defun p (x)
  (format t "~a~%" x)
  x)

;; longest common subsequence
(defun lcs-length (str1 str2)
  (let ((c (make-array `(,(1+ (length str1)) ,(1+ (length str2))) :initial-element 0)))
    (iter (for i from 1 to (length str1))
	  (iter (for j from 1 to (length str2))
		(setf (aref c i j) (if (char= (char str1 (1- i))
					      (char str2 (1- j)))
				       (1+ (aref c (1- i) (1- j)))
				       (max (aref c (1- i) j)
					    (aref c i (1- j)))))))
    ;; (format t "~a~%" c)
    (aref c (length str1) (length str2))))

(defparameter test-goals (make-hash-table :test #'equal))
(setf (gethash 1 test-goals) (make-instance 'goal :title "project" :id 1))
(setf (gethash 2 test-goals) (make-instance 'goal :title "goal1" :id 2 :status "donebut"))
(setf (gethash 3 test-goals) (make-instance 'goal :title "goal2" :id 3 :status "done"))
(setf (gethash 4 test-goals) (make-instance 'goal :title "goal3" :id 4))

(defparameter new-plan
  "* project
  * goal0
  * goal2
  * goal3
*project2
  * goal11
  * goal12
  * goal13")

(defun valid-map-p (lst)
  (equal (length lst)
	 (length (remove-duplicates (mapcar #'cadr lst) :test #'equal))))

(defun maximal-map (alist-o-alists)
  (let (res max)
    (labels ((rec (acc lst)
	       ;; (format t "~a~%" acc)
	       (if (not lst)
		   (if (valid-map-p acc)
		       (let ((score (apply #'+ (mapcar #'caddr acc))))
			 (if (or (not max) (>= score max))
			     (setf max score
				   res acc))))
		   (iter (for elt in (cdar lst))
			 (rec `((,(caar lst) ,(car elt) ,(cdr elt)) ,. acc)
			      (cdr lst))))))
      (rec (list) alist-o-alists)
      res)))

(defun most-probable-correspondance (new-goal-lines &optional (goal-hash *goals*))
  (let ((proximity (list)))
    (iter (for (id goal) in-hashtable goal-hash)
	  (push `(,id . ,(list)) proximity)
	  (let ((micro-res (assoc id proximity :test #'equal)))
	    (iter (for (n-spaces status title) in new-goal-lines)
		  (for lineno from 0)
		  (let* ((lcs-len (lcs-length title (goal-title goal)))
			 (prox (/ lcs-len
				  (1+ (abs (- (+ (length title)
						 (length (goal-title goal)))
					      (* 2 lcs-len)))))))
		    (if (> prox 1)
			(push `(,lineno . ,prox) (cdr micro-res)))))))
    (maximal-map proximity)))
	  

(defun hash->assoc (hash)
  (iter (for (key val) in-hashtable hash)
	(collect `(,key . ,val))))

;; OK, now I have a map between lines and ids.
;; I should modify ids found, create new ones, where appropriate
;; delete missing ones
;; and rearrange tree structure

(defun interpret-input (input)
  (most-probable-correspondance (goal-template-parse 'goal-chart input)))

;; OK, so basically I need to write couple of common-lisp functions, which I'll call from emacs

(let ((count 0))
  (defun emacs-incf ()
    (format nil "~a~%" (incf count)))
  (defun emacs-decf ()
    (format nil "~a~%" (decf count))))

(defun emacs-show-all ()
  "Show all goals there are."
  (setf displayed-goals nil
	cur-line 0)
  (display-goals-flat))

(defun emacs-show-notdone (&optional strict-p)
  "Show all goals that are not done."
  (setf displayed-goals nil
	cur-line 0)
  (display-goals-flat (if strict-p
			  (lambda (x)
			    (not (equal (goal-status x)
					"done")))
			  (lambda (x)
			    (not (cl-ppcre:all-matches "done"
						       (goal-status x)))))))
  

(defparameter *flexoplan-port* 4006)

(defun start-server (&optional (port *flexoplan-port*))
  (swank:create-server :port port))
  

