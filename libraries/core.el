;;; -*- lexical-binding:t -*-

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
(defun dan/garbage-collecting-strategy-after-init-hook ()
  "Adopt a sneaky garbage collection strategy of waiting until idle time to collect staving off the collector while the user is working."
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold 16777216
        gcmh-verbose nil
        gc-cons-percentage 0.1
        file-name-handler-alist dan/last-file-name-handler-alist))

;;;###autoload
(defun dan/throw-error-after-startup (error-message)
  ;; FIXME There is definetly a better way to do this, somehow. Maybe use `window-setup-hook' instead?
  "Throw an error after Emacs has finished loading with ERROR-MESSAGE."
  (add-hook 'after-init-hook #'(lambda ()
                                    (error error-message))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AUTOLOAD LIBRARIES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun dan/load-library (library)
  "Load a LIBRARY from the `dan/library-list'.

First, the library is resolved into a directory. Then, a list of files with a '.el' file extension contained in that directory is created. Finally, each of the files in the list are loaded into Emacs, in no particular order, checking if byte-compiled versions exist and loading them instead in the case they do."
  (let* ((dir (concat dan/libraries-directory (symbol-name library) "/"))
         (files (directory-files dir t "\\.el$")))
    (mapc #'dan/load-elisp-file files)))

;;;###autoload
(defun dan/load-elisp-file (file)
  "Helper function that recieves a FILE with '.el' extension and will load it into Emacs, checking if a bytecompiled version exist, which it will load instead."
  (load (string-remove-suffix ".el" file)) nil 'nomessage)

;; FIXME Optionally wrap this in a timer function, for profiling.
;;;###autoload
(defun dan/load-config (filename)
  "Load FILENAME inside the config directory."
  (load (concat user-emacs-directory filename) nil 'nomessage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CORE PACKAGE SYSTEM ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; straight.el used by default
(setq straight-use-package-by-default t)

;; use ssh for downloading packages
(setq straight-vc-git-default-protocol 'https)

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

;; Garbage collection optimization.
(defvar dan/last-file-name-handler-alist file-name-handler-alist)

(use-package gcmh
  :custom
  (gc-cons-percentage 0.6)
  (file-name-handler-alist nil)
  :config
  (fset 'after-focus-change-function #'gcmh-idle-garbage-collect)
  (add-hook 'after-init-hook #'dan/garbage-collecting-strategy-after-init-hook)
  (gcmh-mode))

;; Don't load irrelevant bytecode
(setq load-prefer-newer t)

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
    "f y"    #'yank-buffer-filename
    "f p"    `((lambda () (interactive)
                 (find-file ,user-emacs-directory)) :wk "[p]rivate config")
    "b"      '(:ignore t :wk "[b]uffer")
    "b d"    #'kill-current-buffer
    "w"      '(:ignore t :wk "[w]indow")
    "h"      '(:ignore t :wk "[h]elp")
    "h k"    '(:ignore t :wk "[k]eybinds")
    "h k a"  #'general-describe-keybindings
    "h k t"  #'which-key-show-top-level
    "h k k"  #'which-key-show-full-keymap
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
  (dan/leader
    :states  '(normal motion)
    :keymaps 'override
    "w q"    '#'evil-quit)

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

;; Extra functions, powered by ivy.
(use-package counsel
  :demand t
  :commands (counsel-switch-buffer
             counsel-ag
             counsel-fzf
             counsel-find-file
             counsel-recentf
             counsel-find-library
             counsel-faces)
  :general
  (dan/leader
    :states   '(normal motion)
    :keymaps  'override
    "b B"     #'counsel-switch-buffer
    "f a"     #'counsel-ag
    "f f"     #'counsel-fzf
    "f o"     #'counsel-find-file
    "f r"     #'counsel-recentf
    "f l"     #'counsel-find-library
    "f F"     #'counsel-faces)
  (:keymaps
   'global-map [remap execute-extended-command] #'counsel-M-x))

;; Improve the default searching text functionality.
(use-package swiper
  :demand t
  :general
  (:keymaps 'evil-motion-state-map
                      [remap evil-ex-search-forward]       #'swiper
                      [remap evil-ex-search-backward]      #'swiper-all
                      [remap evil-ex-search-word-forward]  #'swiper-thing-at-point
                      [remap evil-ex-search-word-backward] #'swiper-all-thing-at-point))


;; Auto complete
;; (use-package company
;;   :hook
;;   '(after-init . global-company-mode)
;;   :commands company-complete-common company-manual-begin company-grab-line
;;   ;; :bind  (:map company-active-map
;;         ;;       ("C-n" . #'company-select-next)
;;         ;;       ("C-p" . #'company-select-previous)
;;         ;;       ("<tab>" . #'company-complete-common-or-cycle)
;;         ;;  :map company-search-map
;;         ;;       ("C-p" . #'company-select-previous)
;;         ;;       ("C-n" . #'company-select-next))
;;   :custom
;;   (company-begin-commands '(self-insert-command))
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.1)
;;   (company-show-numbers t)
;;   (company-tooltip-align-annotations 't)
;;   (company-global-modes
;;    '(not erc-mode message-mode help-mode gud-mode eshell-mode))
;;   (company-backends '(company-capf company-dabbrev company-dabbrev-code))
;;   (company-frontends
;;    '(company-pseudo-tooltip-frontend
;;      company-echo-metadata-frontend)))

;; FIXME Customize.
(use-package company
  :hook
  '(after-init . global-company-mode)
  ;; :config
  ;; (setq company-clang-insert-arguments nil
  ;;       company-semantic-insert-arguments nil
  ;;       company-rtags-insert-arguments nil
  ;;       lsp-enable-snippet nil)
  ;; (advice-add #'eglot--snippet-expansion-fn :override #'ignore)
  ;; :custom
  ;; (company-require-match nil)
  ;; (company-frontends '(company-tng-frontend
  ;;                      company-pseudo-tooltip-frontend
  ;;                      company-echo-metadata-frontend))
  ;; :general
  ;; (:keymaps 'company-active-map
  ;;   "TAB"   'company-select-next
  ;;   "S-TAB" 'company-select-previous)
  )

;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD LIBRARIES ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defconst dan/libraries-directory (concat user-emacs-directory "libraries/")
  "Directory name containing all the libraries of Dan's configuration.")

(defvar dan/library-list '(
                           utils
                           completion   ;; FIXME Slow
                           system       ;; FIXME Slow
                           evil-plugins ;; FIXME Slow
                           ui           ;; FIXME Slow
                           editor-conf
                           rss
                           org-lib      ;; FIXME Slow
                           pdf
                           lsp
                           haskell
                           elisp
                           yaml
                           vim
                           )
  "List of files which make the core of my config. They will be loaded in sequential order.")

;; ;; Load all the libraries
(mapc #'dan/load-library dan/library-list)
