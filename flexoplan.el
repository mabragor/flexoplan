;;; flexoplan.el --- frontend for CL-FLEXOPLAN planner.

(require 'slime)

(defvar flexoplan-connection nil
  "Connection to the CL-FLEXOPLAN.")

(defun flexoplan-connect (&optional port)
  (setq flexoplan-connection (slime-connect "127.0.0.1" (or port 4006) nil nil)))

(defun flexoplan ()
  (interactive)
  (if (not flexoplan-connection)
      (flexoplan-connect))
  (get-buffer-create "*flexoplan*")
  (flexoplan-switch-to-buffer))
  

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
  (slime-eval `(cl-flexoplan::emacs-commit-changes ,(buffer-string))))

(require 'derived)

(define-derived-mode flexoplan-mode text-mode "Flexoplan"
  "Major mode for interaction with CL-FLEXOPLAN planner.
Special commands:
  \\flexoplan-mode-map}"
  )
(define-key flexoplan-mode-map "\C-ci" 'flexoplan-incf)
(define-key flexoplan-mode-map "\C-cd" 'flexoplan-decf)
(define-key flexoplan-mode-map "\C-cs" 'flexoplan-show-notdone)
(define-key flexoplan-mode-map "\C-cS" 'flexoplan-show-all)
(define-key flexoplan-mode-map "\C-x\C-s" 'flexoplan-commit-changes)
(global-set-key "\C-c\C-f" 'flexoplan)

(defun flexoplan-switch-to-buffer ()
  (interactive)
  (if (equal (buffer-name) "*flexoplan*")
      (switch-to-buffer nil)
    (switch-to-buffer "*flexoplan*")))

(provide 'flexoplan)

