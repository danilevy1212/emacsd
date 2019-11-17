;;; -*- lexical-binding: t -*-


;;; 
;;; Daniel's .emacs file.
;;;

;;; Garbage collection optimization on startup
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;;; General configuration
;;  Look and feel
(load "~/.emacs.d/.config/general" nil nil)

;; navigation preferences
(load "~/.emacs.d/.config/navigation" nil nil)

;; Global keyboard shortcuts
(load "~/.emacs.d/.config/keyboard.el" nil nil)

;;; Elisp utils
;; Extra functions
(load "~/.emacs.d/.config/+functions" nil nil)

;;; Programming languages / super modes
;; Org
(load "~/.emacs.d/.config/org" nil nil)

;; Elisp
(load "~/.emacs.d/.config/elisp" nil nil)


;;; Garbage collection optimization after startup
;; A large gc-cons-threshold will cause freezing and stuttering during long-term 
;; interactive use.
(add-hook 'emacs-startup-hook
          '(lambda ()
             (setq gc-cons-threshold 16777216
                   gc-cons-percentage 0.1
                   file-name-handler-alist last-file-name-handler-alist)))
