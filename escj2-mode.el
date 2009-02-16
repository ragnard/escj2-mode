;;;

;;;;;;; escj2-java: ESC/Java2 Minor Mode
;;;;;;; Version 0.1

;;; Author: Ragnar Dahlén (r.dahlen@gmail.com)
;;;
;;;

(require 'jde)

(defvar escj2-command nil
  "Command for ESC/Java2 executable")

(defvar escj2-command-options nil
  "Options for command")

(defvar escj2-mode-keymap nil
  "Keymap for ESC/Java2 minor mode")

(defvar escj2-output-buffer-name nil
  "Name of output buffer for ESC/Java2") 

;;; Set default values
(progn
  (setq escj2-command "escj"))

(progn 
  (setq escj2-command-options "-loopsafe"))

(progn
  (setq escj2-mode-keymap (make-sparse-keymap))
  (define-key escj2-mode-keymap (kbd "C-c m") 'escj2-verify-method-at-point)
  (define-key escj2-mode-keymap (kbd "C-c v") 'escj2-verify-current-buffer)
  (define-key escj2-mode-keymap (kbd "C-c b v") 'escj2-verify-buffer))

(progn
  (setq escj2-output-buffer-name "*escj2-output*"))

;;; Define Minor Mode
(define-minor-mode escj2-mode
  "Toggle ESC/Java2 mode.
     With no argument, this command toggles the mode.  
     Non-null prefix argument turns on the mode.  
     Null prefix argument turns off the mode."
  nil
  " ESCJ2"
  escj2-mode-keymap
  :group 'escj2)


(defun escj2-verify-current-buffer ()
  "Verify the current buffer"
  (interactive)
  (escj2-verify-buffer (buffer-name (current-buffer))))


(defun escj2-verify-buffer (buffer)
  "Verify the contents of a buffer"
  (interactive "bBuffer: ")
  (let ((file-name (buffer-file-name (get-buffer buffer))))
    (escj2-run-verifier file-name escj2-command-options)))


(defun escj2-verify-method-at-point ()
  "Verify the method at point"
  (interactive)
  (let ((file-name (buffer-file-name (current-buffer)))
	(method (car (jde-parse-get-method-at-point))))
    (escj2-verify-method file-name 
			 (car method) 
			 (cdr method))))


(defun escj2-verify-method (file-name class method)
  "Verify a method"
  (interactive)
  (let ((files (list file-name))
	(args (concat "-Routine " method)))
    (escj2-run-verifier files args)))


(defun escj2-run-verifier (files &rest args)
  "Execute ESC/Java2 verifier

   Run the verifier for the java source files in FILES, passing
   it the arguments in ARGS.
  "
  (switch-to-buffer-other-window escj2-output-buffer-name)
  (erase-buffer)
  (goto-char (point-min))
  (let ((program-args (append files args)))
    (eval `(start-process "escj"
		    escj2-output-buffer-name 
		    escj2-command file-name 
		    ,@program-args))))
