;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CORE PACKAGE SYSTEM ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; straight.el used by default
(setq straight-use-package-by-default t)

;; use ssh for downloading packages
(setq straight-vc-git-default-protocol 'ssh)

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

;; Disable warning and error messages at the time of loading packages
(setq use-package-expand-minimally t)

;; Auto-compile emacslisp
(use-package auto-compile
  :config
  (auto-compile-on-save-mode))
