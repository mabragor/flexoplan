;;; flexoplan.el --- frontend for CL-FLEXOPLAN planner.

(require 'slime)

(defvar flexoplan-connection nil
  "Connection to the CL-FLEXOPLAN.")

(defun flexoplan-connect (&optional port)
  (setq flexoplan-connection
	(slime-net-connect "127.0.0.1" (or port 4006)))
  (slime-init-connection-state flexoplan-connection))

(defun flexoplan ()
  (interactive)
  (if (not flexoplan-connection)
      (flexoplan-connect))
  (get-buffer-create "*flexoplan*")
  (with-current-buffer "*flexoplan*"
    (flexoplan-mode)
    (setq slime-buffer-connection flexoplan-connection)
    (if (not (slime-eval '(cl-flexoplan::emacs-flexoplan-mysql-login)))
	(slime-eval `(cl-flexoplan::emacs-flexoplan-mysql-login ,(read-string "MySQL login: "))))
    (if (not (slime-eval '(cl-flexoplan::emacs-flexoplan-mysql-password)))
	(slime-eval `(cl-flexoplan::emacs-flexoplan-mysql-password ,(read-passwd "MySQL password: "))))
    (slime-eval '(cl-flexoplan::%connect))
    (slime-eval '(cl-user::setf cl-flexoplan::*last-id* (cl-flexoplan::get-last-id-from-database))))
  (flexoplan-switch-to-buffer))
  
(defun flexoplan-setup-db-and-tables (user passwd admin-user admin-passwd)
  (interactive (list (read-string "user to access flexoplan db: ")
		     (read-passwd "passwd for this user: " t)
		     (read-string "admin user to create an account: " "root")
		     (read-passwd "passwd of admin user: ")))
  (slime-eval `(cl-flexoplan::setup-mysql-db-and-tables ,admin-user ,admin-passwd ,user ,passwd))
  (message "DB structure successfully created."))

(defun flexoplan-incf ()
  (interactive)
  (with-current-buffer "*flexoplan*"
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert (slime-eval '(cl-flexoplan::emacs-incf)))))


(defun flexoplan-decf ()
  (interactive)
  (with-current-buffer "*flexoplan*"
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert (slime-eval '(cl-flexoplan::emacs-decf)))))

(defmacro flexoplan-redraw (&rest body)
  `(with-current-buffer "*flexoplan*"
     (delete-region (point-min) (point-max))
     (goto-char (point-min))
     ,@body
     (goto-char (point-min))))


(defun flexoplan-show-all ()
  "Show all the goals there are."
  (interactive)
  (flexoplan-redraw
   (insert (slime-eval '(cl-flexoplan::emacs-show-all)))))
  
(defun flexoplan-show-notdone (&optional strict-p)
  "Show all notdone goals. C-u determines, whether to show donebut-goals."
  (interactive "P")
  (flexoplan-redraw
   (insert (slime-eval `(cl-flexoplan::emacs-show-notdone ,(if strict-p t))))))

(defun flexoplan-change-goal-status (&optional status)
  "Change a status of a goal on the current line to the specified one."
  (interactive "P")
  (save-match-data
    (beginning-of-line)
    (if (re-search-forward "^\\( *\\)\\*\\s-*\\(?:(\\([^)]*\\))\\)?\\s-*\\([^\n]*\\)")
	(let* ((new-status (if status
			       (concat "(" status ")")
			     (if (match-string 2) "" "(done)")))
	       (repl-line (concat (match-string 1) "* " new-status
				  (if (not (equal new-status "")) " " "")
				  (match-string 3))))
	  (beginning-of-line)
	  (kill-line)
	  (insert repl-line)
	  (beginning-of-line)
	  (if (not (equal new-status ""))
	      (re-search-forward "^ *\\*\s-*([^)]*")
	    (end-of-line)))
      (error "Current line does not look like a goal description."))))
      
(defun flexoplan-commit-changes ()
  "Sends the whole buffer to the CL-FLEXOPLAN to be analyzed and
smartly saved."
  (interactive)
  (slime-eval `(cl-flexoplan::emacs-commit-changes ,(buffer-string)))
  (message "Changes saved"))

(require 'derived)

(define-derived-mode flexoplan-mode text-mode "Flexoplan"
  "Major mode for interaction with CL-FLEXOPLAN planner.
Special commands:
  \\{flexoplan-mode-map}"
  (make-local-variable 'slime-buffer-connection))
  

;; (define-key flexoplan-mode-map "\C-ci" 'flexoplan-incf)
;; (define-key flexoplan-mode-map "\C-cd" 'flexoplan-decf)
(define-key flexoplan-mode-map "\C-cs" 'flexoplan-show-notdone)
(define-key flexoplan-mode-map "\C-cS" 'flexoplan-show-all)
(define-key flexoplan-mode-map "\C-c\C-c" 'flexoplan-change-goal-status)
(define-key flexoplan-mode-map "\C-x\C-s" 'flexoplan-commit-changes)
(define-key flexoplan-mode-map "\C-xf" 'flexoplan-just-dump-current-plan-to-file)
(define-key flexoplan-mode-map "\C-cp" 'flexoplan-load-project)
(global-set-key "\C-c\C-f" 'flexoplan)

(defun flexoplan-switch-to-buffer ()
  (interactive)
  (if (equal (buffer-name) "*flexoplan*")
      (switch-to-buffer nil)
    (switch-to-buffer "*flexoplan*")))

(defun flexoplan-just-dump-current-plan-to-file (fname)
  "Dump a contents of the *flexoplan* buffer into specified file."
  (interactive (list (expand-file-name (read-file-name
					"File to save to: "
					"~/"
					"flexoplan-dump.txt"))))
  (write-region "---\n" nil fname t)
  (write-region (point-min) (point-max) fname t)
  (write-region "...\n" nil fname t))
					
(defun flexoplan-load-project (&optional project-title)
  "Load goals, belonging to specified project from the database.
If no project specified, load all the goals."
  (interactive (list (read-string "Project title: ")))
  (slime-eval '(cl-flexoplan::%connect))
  (slime-eval `(cl-flexoplan::load-project-from-database ,(if (or (not project-title)
								  (equal project-title ""))
							      nil
							    project-title)))
  (flexoplan-show-all))

(provide 'flexoplan)

