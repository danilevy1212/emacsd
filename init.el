;;; package --- summary:
;;; Commentary:

;;; Base upon which the rest of the config will be built on

;;; Code:
;;; -*- lexical-binding:t -*-

;;; SYSTEM CORE
;; Garbage collection optimization on startup
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;; Config directory, all other dirs are relative to this
(defconst *my-site-lisp-dir* (concat user-emacs-directory "site-lisp/"))
(defconst *my-lisp-dir* (concat user-emacs-directory "lisp/"))
(defconst *my-cache-dir* (concat user-emacs-directory "cache/"))
(defconst *my-backup-dir* (concat user-emacs-directory "backup/"))

;; Create the cache dir
(when (not (file-accessible-directory-p *my-cache-dir*))
  (make-directory *my-cache-dir*))

;; create a centralized backup directory
(when (not (file-accessible-directory-p *my-backup-dir*))
  (make-directory *my-backup-dir*))

;; setup backups directory
(setq backup-directory-alist `(("." . ,*my-backup-dir*))
      make-backup-files t
      backup-by-copying t
      version-control t
      kept-old-versions 2
      kept-new-versions 6
      auto-save-default t
      delete-old-versions t)

;; Load my custom scripts and downloaded scripts to emacs
(setq load-path (append load-path `(,*my-site-lisp-dir* ,*my-lisp-dir*)))

;; Setup default browser to follow links
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (getenv "BROWSER"))

;; Personal info
(setq user-full-name "Daniel Levy Moreno"
      user-mail-address "daniellevymoreno@gmail.com")

;; Typing yes/no is obnoxious when y/n will do
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Customize scratch buffer message
(setq initial-scratch-message ";; Happy hacking ^_^\n")

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; Setup a special file for the customize interface FIXME
(setq custom-file (concat *my-lisp-dir* "custom"))
(when (file-exists-p custom-file)
  (load custom-file nil nil))

;; Call remove trailing whitespaces every time a buffer is saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; When opening a file, always follow symlinks.
(setq vc-follow-symlinks t)

;; When saving a file that starts with `#!', make it executable.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Don't assume that sentences should have two spaces after periods. This ain't a typewriter.
(setq sentence-end-double-space nil)

;; Default tabs are not used for indentation
(setq-default indent-tabs-mode nil)

;; Default tab width
(setq-default tab-width 4)

;; Turn on transient-mark-mode.
(transient-mark-mode t)

;; If some text is selected, and you type some text, delete the selected text and start inserting your typed text.
(delete-selection-mode t)

;; If you save a file that doesn't end with a newline,automatically append one.
(setq require-final-newline t)

;; Ask if you're sure that you want to close Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Add file sizes in human-readable units (KB, MB, etc) to dired buffers.
(setq-default dired-listing-switches "-alh")

;; Turn on syntax highlighting whenever possible."
(global-font-lock-mode t)

;; When something changes a file, automatically refresh the buffer containing that file so they can't get out of sync.
(global-auto-revert-mode t)

;; When middle-clicking the mouse to yank from the clipboard, insert the text where point is, not where the mouse cursor is.
(setq mouse-yank-at-point t)

;; Garbage collection
(require 'gcmh)
(add-hook 'pre-command-hook (gcmh-mode +1))
(with-eval-after-load 'gcmh
  (defun my-garbage-collecting-strategy-after-init-hook ()
    "Adopt a sneaky garbage collection strategy of waiting until idle time to collect staving off the collector while the user is working."
    (setq gcmh-idle-delay 10
          gcmh-high-cons-threshold 16777216
          gcmh-verbose nil
          gc-cons-percentage 0.1
          file-name-handler-alist last-file-name-handler-alist))
  (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect)
  (add-hook 'after-init-hook #'my-garbage-collecting-strategy-after-init-hook))

;;; Load my Elisp utils
(load (concat *my-lisp-dir* "+functions") nil nil)

;;; PACKAGE SYSTEM CORE
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

;; load prefers the newest version of a file
(setq load-prefer-newer t)

;; install use-package
(straight-use-package 'use-package)

;; Disable warning and error messages at the time of loading packages
(setq use-package-expand-minimally t)

;; Auto-compile emacslisp
(use-package auto-compile
  :config
  (auto-compile-on-save-mode)
  ;; FIXME Uncomment once setup is a bit more stable
  ;; (auto-compile-on-load-mode)
  )

;;; KEYBINDING CORE
;; Leader key
(defvar *my-leader-key* "SPC" "Keymap for \"leader key\" shortcuts.")

;; Local leader key
(defvar *my-local-leader-key* "SPC SPC" "Keymap for \"local leader key\" shortcuts.")

;; (defvar *my-local-leader-key* (concat *my-leader-key* "m") "Keymap for \"local leader key\" shortcuts.")
;; Framework for all keybindings
;; FIXME
(use-package general
  :config
  ;; Advise define-key to automatically unbind keys when necessary.
  (general-auto-unbind-keys)

  ;; Definer for global leader keybindings
  (general-create-definer my-leader-def :prefix (symbol-value '*my-leader-key*))

  ;; Some default bindings (using builtins)
  (my-leader-def
    :states '(normal motion visual)
    :keymaps 'override
    "f"      '(:ignore t :wk "[f]ile")
    "f f"    #'find-file
    "f F"    #'find-file-other-window
    "f y"    #'yank-buffer-filename
    "f p"    `((lambda () (interactive)
                (find-file ,user-emacs-directory)) :wk "private config")
    "b"      '(:ignore t :wk "[b]uffer")
    "b d"    #'kill-current-buffer
    "w"      '(:ignore t :wk "[w]indow")
    "w C"    #'kill-buffer-and-window)

  ;; Definer for local leader keybindings
  (general-create-definer my-local-leader-def :prefix (symbol-value '*my-local-leader-key*))

  ;; leader m for mode specific bindings (a la spacemacs)
  (my-local-leader-def
    :states '(normal motion visual)
    :keymaps 'override
    "" '(:ignore t :wk "[SPC]ific to local/mode")))

;; FIXME
;; (use-package hydra)

;; Vim mode for emacs
(use-package evil
  :init
  (setq evil-want-keybinding                  nil
        evil-search-module                    'evil-search
        evil-vsplit-window-right              t
        evil-indent-convert-tabs              t
        evil-split-window-below               t
        evil-ex-search-vim-style-regexp       t
        evil-shift-round                      nil
        evil-want-C-u-scroll                  t)
  :general
  (:states 'normal :keymaps 'override
           ;; Remove highlighted sections with ctrl + l
           "C-l" #'evil-ex-nohighlight
           ;; Universal argument mapped to M-u globally
           "M-u" #'universal-argument)
  (my-leader-def
    :states '(normal motion)
    :keymaps 'override
    "w c"    '#'evil-quit)
  :config
  (evil-mode +1))

;;; PRESENTATION CORE
;; Get that ugly thing out of my sight
(when window-system
  (progn
    (tool-bar-mode -1)
    (menu-bar-mode -1)))

;; Sent font FIXME nicer fonts please
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-face-attribute 'default nil :font "Ubuntu Mono" :height 120)
(set-fontset-font t 'latin "Ubuntu Mono")

;; Disable tool and scrollbars.
(unless (assq 'menu-bar-lines default-frame-alist)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

;; Precise window and frame resizing
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; No splash screen on init
(setq inhibit-splash-screen t)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1)

;; always avoid GUI
(setq use-dialog-box nil)

;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(setq x-gtk-use-system-tooltips nil)

;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 160
      split-height-threshold nil)

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether it has a noteable affect on Linux and Mac hasn't
;; been determined, but we inhibit it there anyway.
(setq inhibit-compacting-font-caches t)

;; Visually indicate matching pairs of parentheses."
(show-paren-mode t)
(setq show-paren-delay 0.05)

;; When you perform a problematic operation, flash the screen instead of ringing the terminal bell.
(setq visible-bell t)

;; Set the default line length to 80.
(setq-default fill-column 80)

;; focus moves to help window
(setq help-window-select t)

;; Describe what each key does while typing
(use-package which-key
  :custom
  (which-key-idle-delay 0.1)
  (which-key-idle-secondary-delay 0.05)
  (which-key-show-early-on-C-h t)
  (which-key-allow-evil-operators t)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location '(bottom right))
  (which-key-show-prefix t)
  (which-key-show-remaining-keys t)
  :config
  (my-local-leader-def
    :states '(normal motion)
    :keymaps 'override
    "h"     '(:ignore t :wk "[h]elp")
    "h k"   '(:ignore t :wk "[k]eybinds")
    "h k M" #'which-key-show-full-major-mode
    "h k m" #'which-key-show-minor-mode-keymap
    "h k t" #'which-key-show-top-level
    "h k k" #'which-key-show-full-keymap)
  (which-key-mode))

;;; UTILS CORE
(use-package xref
  :custom
  (xref-prompt-for-identifier nil))

;; FIXME Modules should be loaded in some other way, maybe through env variables?

;; Editor FIXME Rename
(load-config "my-editor")

;; Look and feel FIXME
(load-config "generalconf")

;; Navigation preferences
(load-config "navigation")

;;; Programming languages / super modes
;; Org
(load-config "my-org")

;; vimscript + vimrc syntax highlight
(load-config "vim")

;; Elisp
(load-config "elisp")

;; Haskell
(load-config "haskell")

;; Python
(load-config "python")

;; Yaml
(load-config "yaml")

;; Json
(load-config "json-conf")

;; c-c++
(load-config "c_c++")

;; web
(load-config "web")

;; latex
(load-config "latex")

(provide 'config)
;;; config.el ends here
