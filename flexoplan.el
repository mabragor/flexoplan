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
  (with-current-buffer "*flexoplan*"
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert (slime-eval '(cl-flexoplan::emacs-incf)))))


(defun flexoplan-decf ()
  (with-current-buffer "*flexoplan*"
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert (slime-eval '(cl-flexoplan::emacs-decf)))))


(defvar flexoplan-mode-hook nil)

(defvar flexoplan-mode-map nil)

(if flexoplan-mode-map
    nil
  (setq flexoplan-mode-map (make-sparse-keymap)))

(provide 'flexoplan)

(flexoplan-connect)

flexoplan-connection

;; OK, now I need a function, that creates a buffer
;; Now I need to write a mode, to tie it automatically to buffer and to write a keybindins for this mode
