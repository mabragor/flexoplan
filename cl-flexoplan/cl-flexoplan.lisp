;;;; cl-flexoplan.lisp

(in-package #:cl-flexoplan)

(cl-interpol:enable-interpol-syntax)
(file-enable-sql-reader-syntax)

(defparameter *last-id* -1)
(defparameter *goals* (make-hash-table :test #'equal))

;;; Now we implement simple in-memory storage of these "goals" (know as "tickets" elsewhere).
(def-view-class goal ()
  ((goal-id 
    :db-kind :key
    :db-constraints :not-null
    :type integer
    :initarg :id
    :accessor goal-id
    :initform (incf *last-id*))
   (parent-goal-id
    :type integer
    :initarg :pid
    :initform nil
    :accessor pid)
   (title :initform (format nil "untitled")
	  :type (string 255)
	  :initarg :title
	  :accessor goal-title)
   (status :type (string 255)
	   :initform nil
	   :initarg :status
	   :accessor goal-status)
   ;; (description :initform nil :initarg :description :accessor goal-description)
   ;; (subgoals :initform nil :initarg :subgoals :accessor goal-subgoals)
   ;; (priority :initform  5 :initarg :priority :accessor goal-priority)))
   (master-goal
    :reader master-goal
    :db-kind :join
    :db-info (:join-class goal
			  :home-key parent-goal-id
			  :foreign-key goal-id
			  :set nil))
   (sub-goals
    :reader sub-goals
    :db-kind :join
    :db-info (:join-class goal
			  :home-key goal-id
			  :foreign-key parent-goal-id
			  :set t))
   (indent
    :reader goal-indent
    :db-kind :virtual))
  (:base-table goals))

(defun drop-all-goals ()
  (setf *last-id* -1)
  (clrhash *goals*)
  (clrhash displayed-goals)
  (clrhash displayed-goal-line))

(defun add-goal (title status) ; &optional append-to)
  "For now very naive version of ADD-GOAL."
  (let ((goal (make-instance 'goal :title title :status status)))
    (setf (gethash (goal-id goal) *goals*) goal)
    ;; (if append-to
    ;; 	(push goal (goal-subgoals (gethash append-to *goals*))))
    (goal-id goal)))

(defun display-goal (goal)
  (format nil "~a* ~a~a~%" ; ~{~a~}"
	  (make-string (goal-indent goal) :initial-element #\space)
	  (with-slots (status) goal
	    (if (and status
		     (not (equal status ""))
		     (not (equal status "open")))
		#?"($((goal-status goal))) "
		""))
	  (goal-title goal)))
	  ;; (let ((indent (+ indent 2)))
	  ;;   (mapcar #'display-goal (reverse (goal-subgoals goal))))))

(defparameter displayed-goals (make-hash-table :test #'equal)
  "Map between lines and ids of currently displayed goals.")
(defparameter displayed-goal-line (make-hash-table :test #'equal)
  "Map of line-number to goal-id, for goals currently displayed.")
(defparameter displayed-project-id nil
  "ID of a project currently being displayed.")
(defparameter cur-line 0
  "Line, which we are currently trying to display.")

(defun display-goals-flat (&optional
			     (predicate #'identity))
  (with-output-to-string (stream)
    (iter (with i = -1)
	  (for (key val) in-hashtable *goals*)
	  (if (not (funcall predicate val))
	      (next-iteration))
	  (setf (gethash key displayed-goals) cur-line)
	  (setf (gethash (incf i) displayed-goal-line) key)
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

(defun load-test-goals ()
  (drop-all-goals)
  (add-goal "project" "")
  (add-goal "goal1" "donebut")
  (add-goal "goal2" "done")
  (add-goal "goal3" ""))

;; (defparameter test-goals (make-hash-table :test #'equal))
;; (setf (gethash 1 test-goals) (make-instance 'goal :title "project" :id 1))
;; (setf (gethash 2 test-goals) (make-instance 'goal :title "goal1" :id 2 :status "donebut"))
;; (setf (gethash 3 test-goals) (make-instance 'goal :title "goal2" :id 3 :status "done"))
;; (setf (gethash 4 test-goals) (make-instance 'goal :title "goal3" :id 4))

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
	       ;; (format t "Acc: ~a~%" acc)
	       (if (not lst)
		   (let ((acc-wo-removes (remove-if (lambda (x)
						      (eql (cadr x) nil))
						    acc)))
		     (if (valid-map-p acc-wo-removes)
			 (let ((score (apply #'+ (mapcar #'caddr acc-wo-removes))))
			   (if (or (not max) (>= score max))
			       (setf max score
				     res acc-wo-removes)))))
		   (iter (for elt in `((nil . 0) ,.(cdar lst)))
			 (rec `((,(caar lst) ,(car elt) ,(cdr elt)) ,. acc)
			      (cdr lst))))))
      (rec (list) alist-o-alists)
      res)))

(defun most-probable-correspondance (new-goal-lines &optional (goal-hash *goals*))
  (let ((proximity (list)))
    (iter (for (id goal) in-hashtable goal-hash)
	  (if (not (gethash id displayed-goals))
	      (next-iteration))
	  (push `(,id . ,(list)) proximity)
	  (let ((micro-res (assoc id proximity :test #'equal)))
	    (iter (for (n-spaces status title) in new-goal-lines)
		  (for lineno from 0)
		  (let* ((lcs-len (lcs-length title (goal-title goal)))
			 (prox (/ lcs-len
				  (1+ (abs (- (+ (length title)
						 (length (goal-title goal)))
					      (* 2 lcs-len)))))))
		    ;; (format t "Prox: ~a~%" prox)
		    (if (> prox 1)
			(push `(,lineno . ,prox) (cdr micro-res)))))))
    ;; (format t "Proximity: ~a~%" proximity)
    (maximal-map proximity)))
	  

(defun hash->assoc (hash)
  (iter (for (key val) in-hashtable hash)
	(collect `(,key . ,val))))

(defun interpret-input (input)
  (let (modified-ids removed-ids new-ids)
    (let* ((new-lines (goal-template-parse 'goal-chart input))
	   (corr (most-probable-correspondance new-lines))
	   (parents (figure-out-parents new-lines)))
      ;; (format t "New lines: ~a~%" new-lines)
      ;; (format t "Corr: ~a~%" corr)
      (iter (for (id lineno nil) in corr)
	    (push `(,id ,lineno ,@(nth lineno new-lines)) modified-ids))
      (iter (for (key nil) in-hashtable displayed-goals)
	    (if (not (find key corr :key #'car :test #'equal))
		(push key removed-ids)))
      (iter (for line in new-lines)
	    (for lineno from 0)
	    (if (not (find lineno corr :key #'cadr :test #'equal))
		(push `(,lineno ,. line) new-ids)))
      (values corr modified-ids removed-ids new-ids parents))))

(defun figure-out-parents (lines)
  (let (parents-dyn res)
    ;; Here's what happens, when really recursive algorithm with a dynamic variable gets
    ;; written as an iterative one. However, I was unable to figure out how the recursive form
    ;; looks right away.
    (iter (for line on lines)
	  (for i from 0)
	  ;; (format t "line: ~a~%" (car line))
	  ;; (format t "parents: ~a~%" parents-dyn)
	  (iter (while (and parents-dyn (<= (caar line) (caar parents-dyn))))
		(pop parents-dyn))
	  ;; (format t "parents-after: ~a~%" parents-dyn)
	  (push `(,i . ,(cdar parents-dyn)) res)
	  (push `(,(caar line) . ,i) parents-dyn))
    res))

(defparameter test-lines
  "* a
* b
  * c
    * d
   * e
  * f
   * g
   * h ")

(let ((count 0))
  (defun emacs-incf ()
    (format nil "~a~%" (incf count)))
  (defun emacs-decf ()
    (format nil "~a~%" (decf count))))

(defun emacs-show-all ()
  "Show all goals there are."
  (clrhash displayed-goals)
  (setf cur-line 0)
  (display-goals-flat))

(defun emacs-show-notdone (&optional strict-p)
  "Show all goals that are not done."
  (clrhash displayed-goals)
  (setf cur-line 0)
  (display-goals-flat (if strict-p
			  (lambda (x)
			    (not (equal (goal-status x)
					"done")))
			  (lambda (x)
			    (not (cl-ppcre:all-matches "done"
						       (goal-status x)))))))

(defmacro fart (&body vars)
  `(progn ,@(mapcar (lambda (x)
		      `(format t ,(strcat (string-downcase x) ": ~a~%") ,x))
		    vars)))

(defmacro if-dyn (sym then &optional else)
  `(if (and (boundp ',sym) ,sym)
       ,then
       ,else))

(defmacro! if-debug (&body syms)
  `(if-dyn ,e!-debug
	   (fart ,@syms)))

(defun emacs-commit-changes (new-goals-text)
  (format t new-goals-text)
  (multiple-value-bind (corr modified removed new parents) (interpret-input new-goals-text)
    (if-debug corr modified removed new parents)
    (clrhash displayed-goal-line)
    (clrhash displayed-goals)
    (iter (for id in removed)
	  (delete-instance-records (gethash id *goals*) :database db-connection)
	  (remhash id *goals*))
    (iter (for (new-lineno new-indent new-status new-title) in new)
	  (let ((new-id (add-goal new-title new-status)))
	    (setf (gethash new-id displayed-goals) new-lineno)
	    (setf (gethash new-lineno displayed-goal-line) new-id)))
    (iter (for (id new-lineno new-indent new-status new-title) in modified)
	  (let ((goal (gethash id *goals*)))
	    (with-slots (title status) goal
	      (setf title new-title
		    status new-status))
	    (setf (gethash id displayed-goals) new-lineno)
	    (setf (gethash new-lineno displayed-goal-line) id)))
    (iter (for (lineno id) in-hashtable displayed-goal-line)
	  (setf (pid (gethash id *goals*))
		(or (gethash (cdr (assoc lineno parents :test #'equal))
			     displayed-goal-line)
		    displayed-project-id))
	  (update-records-from-instance (gethash id *goals*) :database db-connection))))

(defparameter *flexoplan-port* 4006)

(defun start-server (&optional (port *flexoplan-port*))
  (swank:create-server :port port))
  
(defparameter db-connection nil)

;; Utilities that connect to the database

(defun %probe-database (connection-spec &key database-type)
  (handler-case (probe-database connection-spec :database-type database-type)
    (sql-connection-error (e)
      (if (equal (sql-error-error-id e) 1049)
	  nil
	  (error e)))))

(defun %create-database
    (connection-spec &key database-type
		       if-not-exists)
  (if (or (not if-not-exists)
	  (not (%probe-database connection-spec
				:database-type database-type)))
      (create-database connection-spec :database-type database-type)))

(defvar flexoplan-mysql-login nil
  "Login to MySQL, should be redefined, e.g in config.")
(defvar flexoplan-mysql-password nil
  "Password to MySQL, should be redefined, e.g in config.")

(defun emacs-flexoplan-mysql-login (&optional (x nil x-p))
  (if x-p
      (setf flexoplan-mysql-login x)
      flexoplan-mysql-login))
(defun emacs-flexoplan-mysql-password (&optional (x nil x-p))
  (if x-p
      (setf flexoplan-mysql-password x)
      flexoplan-mysql-password))

(defun %connect (&optional force-reconnect)
  ;; (%create-database '("localhost" "flexoplan" "root" "trga%")
  ;; 		    :database-type :mysql
  ;; 		    :if-not-exists t)
  (when (and force-reconnect db-connection)
    (disconnect :database db-connection :error nil)
    (setf db-connection nil))
  (when (not db-connection)
    (setf db-connection (clsql:connect `("localhost"
					 "flexoplan"
					 ,(or flexoplan-mysql-login
					      (error (strcat "You should specify MySQL login via "
							     "FLEXOPLAN-MYSQL-LOGIN variable or via "
							     "EMACS-FLEXOPLAN-MYSQL-LOGIN setter-function.")))
					 ,(or flexoplan-mysql-password
					      (error (strcat "You should specify MySQL password via "
							     "FLEXOPLAN-MYSQL-PASSWORD variable or via "
							     "EMACS-FLEXOPLAN-MYSQL-PASSWORD setter-function."))))
				       :database-type :mysql)))
  t)

(defun init-tables ()
  (create-view-from-class 'goal))

(defun get-last-id-from-database ()
  (let ((res (select [max [goal-id]] :from [goals])))
    (or (caar res) -1)))

(defun load-project-from-database (&optional project-name)
  "Load into memory all goals, that are descendants of a given project-goal.
PROJECT-NAME, if given, should be EQUAL to TITLE of some goal.
If project-name is NIL, all goals are loaded."
  (drop-all-goals)
  (setf *last-id* (get-last-id-from-database))
  (setf displayed-project-id (a:aif (car (select 'goal :where [= [slot-value 'goal 'title] project-name]))
				    (goal-id (car it))))
  (let ((all-goals (recursively-load-goals-from-database displayed-project-id)))
    (iter (for goal in all-goals)
	  (setf (gethash (goal-id goal) *goals*) goal))
    t))

(defun load-goals-from-database (&optional (master-goal-id nil supplied-p))
  "Load from a database all the goals, whose master goal is has a MASTER-GOAL-ID.
If MASTER-GOAL-ID is not specified, fetches all goals.
If it explicitly is specified as NIL, then fetches only top-level
goals (for which MASTER-GOAL-ID is NULL)."
  (mapcar #'car
	  (cond
	    ((not supplied-p) (select 'goal :database db-connection))
	    ((not master-goal-id) (select 'goal
					  :where [null [slot-value 'goal 'parent-goal-id]]
					  :database db-connection))
	    (t (select 'goal
		       :where [= [slot-value 'goal 'parent-goal-id] master-goal-id]
		       :database db-connection)))))
    
(defun recursively-load-goals-from-database (&optional master-goal-id (indent 0))
  (let ((this-level (load-goals-from-database master-goal-id)))
    (iter (for goal in this-level)
	  (setf (slot-value goal 'indent) indent)
	  (collect goal)
	  (appending (recursively-load-goals-from-database (goal-id goal) (+ 2 indent))))))

(in-package #:clesh)

(defun escript (str &key (program *shell*))
  (multiple-value-bind (stdout stderr errno)
      (script str :program program)
    (if (zerop errno)
	(values stdout stderr errno)
	(error "Script exited with errno ~a: stderr: ~a" errno stderr))))

(in-package #:cl-flexoplan)

(defun setup-mysql-db-and-tables (admin-user admin-passwd user passwd)
  "Setup database, user and tables, needed for persistence of goals."
  (let ((sql-cmd #?"create database if not exists flexoplan
    default charset = utf8;
grant all privileges on flexoplan.* to '$(user)'@'localhost'
    identified by '$(passwd)';"))
    (clesh::escript #?"echo \"$(sql-cmd)\" | mysql -u$(admin-user) -p$(admin-passwd)"))
  (let ((flexoplan-mysql-login user)
	(flexoplan-mysql-password passwd))
    (%connect)
    (unwind-protect (handler-case (create-view-from-class 'goal :database db-connection)
		      (sql-database-data-error (e)
			(let ((it (sql-error-error-id e)))
			  (if (equal it 1050)
			      (warn "Table already exists, hence not created.")
			      (error e)))))
      (disconnect :database db-connection)
      (setf db-connection nil))))
    
      
