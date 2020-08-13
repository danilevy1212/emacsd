;;; -*- lexical-binding:t -*-

;; TODO core takes almost .5s to load, see if I can make it faster.

;;;;;;;;;;;;;;;;;;;;;;
;;; AUTOLOAD UTILS ;;;
;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defmacro dan/inhibit-evil-insert-state (keymap)
  "Unmap insertion keys from normal state of KEYMAP.

This is particularly useful for read-only modes. Inspired by `evil-collection-inhibit-insert-state'."
  `(general-unbind 'normal keymap
    :with #'ignore
    [remap evil-append]
    [remap evil-append-line]
    [remap evil-insert]
    [remap evil-insert-line]
    [remap evil-change]
    [remap evil-change-line]
    [remap evil-substitute]
    [remap evil-change-whole-line]
    [remap evil-delete]
    [remap evil-delete-line]
    [remap evil-delete-char]
    [remap evil-delete-backward-char]
    [remap evil-replace]
    [remap evil-replace-state]
    [remap evil-open-below]
    [remap evil-open-above]
    [remap evil-paste-after]
    [remap evil-paste-before]
    [remap evil-join]
    [remap evil-indent]
    [remap evil-shift-left]
    [remap evil-shift-right]
    [remap evil-invert-char]))

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


;;;###autoload
(defun dan/throw-error-after-startup (error-message)
  "Throw an error after Emacs has finished loading with ERROR-MESSAGE."
  (add-hook 'after-init-hook #'(lambda ()
                                    (error error-message))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUTOLOAD LIBRARIES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME Optionally wrap this in a timer function, for profiling.
;;;###autoload
(defun dan/load-library (library &optional timeit)
  "Load a LIBRARY from the `dan/library-list'.

If TIMEIT is declared and given a value different than nil, then report
how long each package took to load."
  (let* ((dir (concat dan/libraries-directory (symbol-name library) "/"))
         (files (directory-files dir t "\\.el$")))
    (mapc #'dan/load-elisp-file files)))

;; FIXME Optionally wrap this in a timer function, for profiling.
;;;###autoload
(defun dan/load-elisp-file (file &optional timeit)
  "Load a FILE with '.el' extension into Emacs.

If TIMEIT is declared and given a value different than nil, then return how long
it took for the file to load in miliseconds."
  (load (string-remove-suffix ".el" file)) nil 'nomessage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CORE PACKAGE SYSTEM ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Straight.el used by default
(setq straight-use-package-by-default t)

;; Use ssh for downloading packages
(setq straight-vc-git-default-protocol 'https)

;; Repair flycheck integration with straight.el
(setq straight-fix-flycheck t)

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(straight-use-package 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GARBAGE COLLECTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO Study and understand what this is doing.
(defun dan/garbage-collecting-strategy ()
  "Adopt a sneaky garbage collection strategy of waiting until idle time to collect starving off the collector while the user is working."
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold 16777216
        gcmh-verbose nil
        gc-cons-percentage 0.1
        ;; FIXME Why this?!?!
        file-name-handler-alist dan/last-file-name-handler-alist))

;; Garbage collection optimization.
(defvar dan/last-file-name-handler-alist file-name-handler-alist)

(use-package gcmh
  :custom
  (gc-cons-percentage 0.9)
  (file-name-handler-alist nil)
  :config
  (fset 'after-focus-change-function #'gcmh-idle-garbage-collect)
  (run-with-timer 2 nil #'dan/garbage-collecting-strategy)
  (gcmh-mode))

;;;;;;;;;;;;;;;;;;;;;;
;;; CORE VARIABLES ;;;
;;;;;;;;;;;;;;;;;;;;;;

;; Create the cache dir
(defconst dan/cache-dir (concat user-emacs-directory "cache/")
  "Directory where cache files will be stored.")
(when (not (file-accessible-directory-p dan/cache-dir))
  (make-directory dan/cache-dir))

;; Create a centralized backup directory
(defconst dan/backup-dir (concat user-emacs-directory "backup/")
  "Directory where the backup files will be stored.")
(when (not (file-accessible-directory-p dan/backup-dir))
  (make-directory dan/backup-dir))

;; TODO For optimization, wrap all of this in a function that is called in the
;; necessary parts of the config (getenv is slow yo)
;; Create a centralized backup directory
(defconst dan/cloud-dir (concat (getenv "HOME") "/Cloud/")
  "Directory where the cloud service stores files.")
(when (not (file-accessible-directory-p dan/cloud-dir))
  (dan/throw-error-after-startup (format
                                  "Cloud directory %s does not exist, please assign dan/cloud-dir to a real directory"
                                  dan/cloud-dir)))

;;;;;;;;;;;;;;;;;;
;;; CORE UTILS ;;;
;;;;;;;;;;;;;;;;;;


;; Don't load irrelevant bytecode
(setq load-prefer-newer t)

;; Manage those dependencies!
(use-package use-package-ensure-system-package
  :straight
  (:flavor melpa))

;; Leader key
(defconst dan/leader-key "SPC" "Keymap for \"leader key\" shortcuts.")

;; Local leader key
(defconst dan/local-leader-key "," "Keymap for \"local leader key\" shortcuts.")

;; Framework for all keybindings
(use-package general
  :config
  ;; Advise define-key to automatically unbind keys when necessary.
  (general-auto-unbind-keys)

  ;; Definer for global leader keybindings
  (general-create-definer dan/leader :prefix (symbol-value 'dan/leader-key))

  ;; Some default bindings (using builtins)
  (dan/leader
    :states  '(normal motion visual)
    :keymaps 'override
    "f"      '(:ignore t :wk "[f]ile")
    "f y"    #'dan/yank-buffer-filename
    "f p"    `((lambda () (interactive)
                 (find-file ,user-emacs-directory)) :wk "[p]rivate config")
    "b"      '(:ignore t :wk "[b]uffer")
    "b d"    #'kill-current-buffer
    "w"      '(:ignore t :wk "[w]indow")
    "h"      '(:ignore t :wk "[h]elp")
    "h k"    '(:ignore t :wk "[k]eybinds")
    "h k a"  #'general-describe-keybindings
    "m"      (general-simulate-key "," :which-key "[m]ajor-mode"))

  ;; Definer for local leader keybindings
  (general-create-definer dan/local-leader :prefix (symbol-value 'dan/local-leader-key))

  ;; leader m for mode specific bindings (a la spacemacs)
  (dan/local-leader
    :states '(normal motion visual)
    :keymaps 'override
    ""       '(:ignore t :wk "[m]ajor mode")
    "h"      '(:ignore t :wk "[h]elp")
    "h k"    '(:ignore t :wk "[k]eybinds"))

  ;; switch to help org file automatically
  (advice-add #'general-describe-keybindings :after #'other-window))

;; Can'live without vim-keys man!
(use-package evil
  :demand t
  :init
  (setq evil-want-keybinding                  nil
        evil-vsplit-window-right              t
        evil-indent-convert-tabs              t
        evil-split-window-below               t
        evil-ex-search-vim-style-regexp       t
        evil-shift-round                      nil
        evil-want-C-u-scroll                  t
        evil-cross-lines                      t)
  :general
  (:states '(normal motion visual) :keymaps 'override
           ;; Remove highlighted sections with ctrl + l
           "C-l" #'evil-ex-nohighlight
           ;; Universal argument mapped to M-u globally
           "M-u" #'universal-argument)

  ;; Keychord to get out of insert mode
  (general-define-key :state 'insert
                      "j" (general-key-dispatch 'self-insert-command
                            :timeout 0.25
                            "k" 'evil-normal-state))
  (general-define-key :state 'insert
                      "k" (general-key-dispatch 'self-insert-command
                            :timeout 0.25
                            "j" 'evil-normal-state))
  :config
  (evil-mode +1)
  :custom
  (evil-search-module                  'evil-search)
  (evil-ex-search-persistent-highlight  nil)
  (evil-ex-search-highlight-all         t)
  (evil-symbol-word-search              t))

;; General search engine. FIXME Customize
(use-package ivy
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  ;; Define the optimal height of the ivy buffer.
  (ivy-height-alist
   '(((t lambda (_caller) (/ (window-height) 4)))))
  (ivy-height 15)
  :config
  (ivy-mode +1)
  :general
  (:states '(motion normal)
           "<s-up>"   #'ivy-push-view
           "<s-down>" #'ivy-switch-view)
  (dan/leader
    :states   '(normal motion)
    :keymaps  'override
    "b b"      #'switch-to-buffer
    "i"        '(:ignore t :wk "[i]vy")
    "i r"      #'ivy-resume))

;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD LIBRARIES ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defconst dan/libraries-directory (concat user-emacs-directory "libraries/")
  "Directory name containing all the libraries of Dan's configuration.")

(defvar dan/library-list '(utils
                           completion   ;; FIXME Slow
                           system       ;; FIXME Slow
                           ui           ;; FIXME Slow
                           text-editor
                           rss
                           org-lib      ;; FIXME Slow
                           pdf
                           lsp
                           haskell
                           elisp
                           yaml
                           rust
                           vim)
  "List of files which make the core of my config. They will be loaded in sequential order.")

;; ;; Load all the libraries
(mapc #'dan/load-library dan/library-list)
