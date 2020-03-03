;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-


;;; Daniel's .emacs file.

;;; Code:
;;; Garbage collection optimization on startup
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;;; General configuration
;;  Look and feel
(load "~/.emacs.d/.config/generalconf" nil nil)

;; navigation preferences
(load "~/.emacs.d/.config/navigation" nil nil)

;; Global keyboard shortcuts
(load "~/.emacs.d/.config/keyboard.el" nil nil)

;;; Elisp utils
;; Extra functions
(load "~/.emacs.d/.config/+functions" nil nil)

;;; Programming languages / super modes

;; Org
(load "~/.emacs.d/.config/orgconf" nil nil)

;; vimscript + vimrc syntax highlight
(load "~/.emacs.d/.config/vim" nil nil)

;; Elisp
(load "~/.emacs.d/.config/elisp" nil nil)

;; Haskell
(load "~/.emacs.d/.config/haskell" nil nil)

;; Python
(load "~/.emacs.d/.config/python" nil nil)

;; Yaml
(load "~/.emacs.d/.config/yaml" nil nil)

;; Json
(load "~/.emacs.d/.config/json" nil nil)

;; c-c++
(load "~/.emacs.d/.config/c_c++" nil nil)

;; web
(load "~/.emacs.d/.config/web" nil nil)

;; latex
(load "~/.emacs.d/.config/latex" nil nil)

;;; Garbage collection optimization after startup
;; A large gc-cons-threshold will cause freezing and stuttering during long-term
;; interactive use.
(add-hook 'emacs-startup-hook
          '(lambda ()
             (setq gc-cons-threshold  20000000
                   gc-cons-percentage 0.1
                   file-name-handler-alist last-file-name-handler-alist)))

(provide 'config)
;;; config.el ends here
