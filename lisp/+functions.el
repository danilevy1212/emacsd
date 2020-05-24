;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;;; Code:
;;;###autoload
(defun dan/only-current-buffer ()
  "Function to kill all other buffers except current one and special ones."
  (interactive)
  (dolist (buffer (delq (current-buffer) (buffer-list)))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (/= (aref name 0) ?\s)
                 (string-match "^[^\*]" name))
        (funcall 'kill-buffer buffer)))))

;;;###autoload
(defun dan/sudo-edit ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

;;;###autoload
(defun dan/yank-buffer-filename ()
  "Yank current buffer's filename to the kill ring, else return nil."
  (interactive)
  (let ((path (buffer-file-name (current-buffer))))
    (message "Inserted in kill ring: %s" (when path
                                           (kill-new path)))))

;;;###autoload
(defun dan/create-directories-recursively ()
  "When saving a file in a directory that doesn't exist, offer to (recursively) create the file's parent directories."
  (when buffer-file-name
    (let ((dir (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p dir))
                 (yes-or-no-p (format "Directory %s does not exist. Create it?" dir)))
        (make-directory dir t)))))
(add-hook 'before-save-hook
          'my/create-directories-recursively)

;;;###autoload
(defun dan/load-config (filename)
  "Load FILENAME inside the config directory."
  (load (concat user-emacs-directory filename) nil nil))

(provide '+functions)
;;; +functions.el ends here