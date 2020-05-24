;;;###autoload
(defun dan/load-dir (dir)
  "Load all the elisp files of a given DIR, non recursively."
  (let ((files (directory-files dir t "\\.el$")))
    (mapc (lambda (file)
              (load (string-remove-suffix ".el" file)) nil 'nomessage) files)))

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
  (load (concat user-emacs-directory filename) nil 'nomessage))

(defconst dan/core-libs '("pacman" "utils" "system" "evil" "ui")
  "List of files which make the core of my config. They will be loaded in sequential appearing order.")

(let ((core-dir (concat user-emacs-directory "core-lib/")))
  (mapc (lambda (filename)
          (load (concat core-dir filename) nil 'nomessage))
        dan/core-libs))
