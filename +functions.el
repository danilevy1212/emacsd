;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;;; Code:
(defun only-current-buffer ()
  "Function to kill all other buffers except current one and special ones."
  (interactive)
  (dolist (buffer (delq (current-buffer) (buffer-list)))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (/= (aref name 0) ?\s)
                 (string-match "^[^\*]" name))
        (funcall 'kill-buffer buffer)))))

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(defun copy-buffer-filename ()
  "Copy current buffer's filename to the kill ring, else return nil."
  (interactive)
  (let ((path (buffer-file-name (current-buffer))))
    (message "%s" (when path
		    (kill-new path)))))

(provide '+functions)
;;; +functions.el ends here
