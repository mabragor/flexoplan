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
  (let ((buf (get-buffer "*flexoplan*")))
    (if (not buf)
	(create-buffer "*flexoplan*"))
    (buffer-name (switch-to-buffer "*flexoplan*"))))
  

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

(provide 'flexoplan)

