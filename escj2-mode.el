;;;
;;;
;;;
;;;
(defvar escj2-command nil
  "Command for ESC/Java2 executable")

(defvar escj2-command-options nil
  "Options for command")

(defvar escj2-mode-keymap nil
  "Keymap for ESC/Java2 minor mode")

(defvar escj2-output-buffer-name nil
  "Name of output buffer for ESC/Java2") 

(progn
  (setq escj2-command "escj"))

(progn 
  (setq escj2-command-options "-loopsafe"))

(progn
  (setq escj2-mode-keymap (make-sparse-keymap))
  (define-key escj2-mode-keymap (kbd "C-c v") 'escj2-verify-current-buffer)
  (define-key escj2-mode-keymap (kbd "C-c b v") 'escj2-verify-buffer))

(progn
  (setq escj2-output-buffer-name "*escj2-output*"))

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
  (interactive)
  (escj2-verify-buffer (buffer-name (current-buffer))))

(defun escj2-verify-buffer (buffer)
  (interactive "bBuffer: ")
  (let ((file-name (buffer-file-name (get-buffer buffer))))
    (switch-to-buffer-other-window escj2-output-buffer-name)
    (goto-char (point-min))
    ;(erase-buffer)
    (start-process "escj" escj2-output-buffer-name escj2-command file-name escj2-command-options)))

