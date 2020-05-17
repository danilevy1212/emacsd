;;; package --- summary:
;;; Commentary:

;;; Base upon which the rest of the config will be built on

;;; Code:
;;; -*- lexical-binding:t -*-

;;;;;;;;;;;;;;;;;;;
;;; INIT SYSTEM ;;;
;;;;;;;;;;;;;;;;;;;

;; When opening a file, always follow symlinks.
(setq vc-follow-symlinks t)

;; Add file sizes in human-readable units (KB, MB, etc) to dired buffers.
(setq-default dired-listing-switches "-alsh")

;; Turn on syntax highlighting whenever possible."
(global-font-lock-mode t)

;; When something changes a file, automatically refresh the buffer containing that file so they can't get out of sync.
(global-auto-revert-mode t)

;; FIXME Separate CORE into files, and these files will be part
;; of core library

;; TODO set up emacs-libs repo
(defvar libs/load-libraries 'all
  "Libraries that will be loaded from danilevy/emacs-libs.")

(defconst libs/libraries-directories-alist
  '((core-lib  . "core-lib/")
    (elisp-lib . "elisp-lib/"))
  "List of alists containing the symbolsa as CAR and relative directories of the libs as CDR.")

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

;; load prefers the newest version of a file
(setq load-prefer-newer t)

;; install use-package
(straight-use-package 'use-package)

;; Disable warning and error messages at the time of loading packages
(setq use-package-expand-minimally t)

;; Auto-compile emacslisp
(use-package auto-compile
  :config
  (auto-compile-on-save-mode))

;;;;;;;;;;;;;;;;;;
;;; CORE UTILS ;;;
;;;;;;;;;;;;;;;;;;

;; Config directory, all other dirs are relative to this
(defconst *my-site-lisp-dir* (concat user-emacs-directory "site-lisp/"))
(defconst *my-lisp-dir* (concat user-emacs-directory "lisp/"))

;; Create the cache dir
(defconst *my-cache-dir* (concat user-emacs-directory "cache/")
  "Directory where cache files will be stored.")
(when (not (file-accessible-directory-p *my-cache-dir*))
  (make-directory *my-cache-dir*))

;; Create a centralized backup directory
(defconst *my-backup-dir* (concat user-emacs-directory "backup/")
  "Directory where the backup files will be stored.")
(when (not (file-accessible-directory-p *my-backup-dir*))
  (make-directory *my-backup-dir*))

;; Load my custom scripts and downloaded scripts to emacs
(setq load-path (append load-path `(,*my-site-lisp-dir* ,*my-lisp-dir*)))

;; Garbage collection optimization on startup
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

;; Garbage collection
;; FIXME Pass this to `use-package'
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
  (fset 'after-focus-change-function #'gcmh-idle-garbage-collect)
  (add-hook 'after-init-hook #'my-garbage-collecting-strategy-after-init-hook))


;; FIXME
;;; Load my Elisp utils
(load (concat *my-lisp-dir* "+functions") nil nil)

;; Convinience functions
(use-package dash)

;; The long lost Emacs string manipulation library.
(use-package s)

;; Leader key
(defconst *my-leader-key* "SPC" "Keymap for \"leader key\" shortcuts.")

;; Local leader key
(defconst *my-local-leader-key* "," "Keymap for \"local leader key\" shortcuts.")

;; Framework for all keybindings
(use-package general
  :config
  ;; Advise define-key to automatically unbind keys when necessary.
  (general-auto-unbind-keys)

  ;; Definer for global leader keybindings
  (general-create-definer my-leader-def :prefix (symbol-value '*my-leader-key*))

  ;; Some default bindings (using builtins)
  (my-leader-def
    :states  '(normal motion visual)
    :keymaps 'override
    "f"      '(:ignore t :wk "[f]ile")
    "f y"    #'yank-buffer-filename
    ;; FIXME make a macro for this?, my/wk-wrap? INSPIRATION https://cestlaz.github.io/posts/using-emacs-31-elfeed-3/
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
  (general-create-definer my-local-leader-def :prefix (symbol-value '*my-local-leader-key*))

  ;; leader m for mode specific bindings (a la spacemacs)
  (my-local-leader-def
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
  (my-leader-def
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
  (evil-ex-search-persistent-highlight nil)
  (evil-ex-search-highlight-all        t)
  (evil-symbol-word-search             t))

;; FIXME Customize!
(use-package hydra
  :custom
  (hydra-look-for-remap t))

;; Typing yes/no is obnoxious when y/n will do
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Ask if you're sure that you want to close Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; Setup a special file for the customize interface.
(setq custom-file (concat *my-lisp-dir* "custom"))
(when (file-exists-p custom-file)
  (load custom-file nil nil))

;; Cache of recently visited files.
(use-package recentf
  :custom
  (recentf-save-file (concat *my-cache-dir* "recentf"))
  (recentf-max-saved-items 30)
  (recentf-max-menu-items  30))

;; setup backups directory
(setq backup-directory-alist `(("." . ,*my-backup-dir*))
      make-backup-files t
      backup-by-copying t
      version-control   t
      kept-old-versions 2
      kept-new-versions 6
      auto-save-default t
      delete-old-versions t)

;;;;;;;;;;;;;;;;;;
;;; COMPLETION ;;;
;;;;;;;;;;;;;;;;;;

;; TODO https://github.com/oantolin/orderless

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
  (my-leader-def
    :states   '(normal motion)
    :keymaps  'override
    "b b"      #'switch-to-buffer
    "i"        '(:ignore t :wk "[i]vy")
    "i r"      #'ivy-resume))

(use-package ivy-hydra
  :custom
  (ivy-read-action-function 'ivy-hydra-read-action)
  :config
  (general-define-key :keymaps 'ivy-mode-map "C-c C-h" #'hydra-ivy/body))

;; Extra functions, powered by ivy.
(use-package counsel
  :general
  (my-leader-def
    :states   '(normal motion)
    :keymaps  'override
    "b B"     #'counsel-switch-buffer
    "f f"     #'counsel-find-file
    "f r"     #'counsel-recentf
    "f l"     #'counsel-find-library
    "f F"     #'counsel-faces)
  (:keymaps
   'global-map [remap execute-extended-command] 'counsel-M-x))

;; Added M-x heuristics.
(use-package amx
  :custom
  (amx-backend 'ivy)
  (amx-save-file (concat *my-cache-dir* "amx-items")))

;; Improve the default searching text functionality.
(use-package swiper
  :config
  (general-define-key :keymaps 'evil-motion-state-map
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

;; (use-package iedit)

;; (use-package evil-iedit-state)

;; https://blog.binchen.org/posts/how-to-use-expand-region-efficiently.html
;; (use-package expand-region)

;;;;;;;;;;;;;;;;;;;;
;;; SYSTEM TOOLS ;;;
;;;;;;;;;;;;;;;;;;;;

;; FIXME Part of core.el
;; Git porcelain
(use-package magit
  :config
  (magit-auto-revert-mode +1)
  :general
  (my-leader-def
    :states '(normal motion)
    :keymaps 'override
    "g"   '(:ignore t :wk "[g]it")
    "g s" #'magit-status))

;;;;;;;;;;;;;;;;;;;
;;; VIM PLUGINS ;;;
;;;;;;;;;;;;;;;;;;;

;; Evil-like keybinds for magit
(use-package evil-magit)

;; FIXME This mode is way too much bloat, keep it for reference, and only pick and choose what I like from it.
;; FIXME Translate the keybindings I like with general.
(use-package evil-collection
  :custom
  (evil-collection-mode-list '(ivy
                               info
                               dired
                               helpful
                               magit
                               minibuffer
                               dashboard
                               company
                               popup
                               help
                               helpful
                               vterm))
  (evil-collection-outline-bind-tab-p           t)
  (evil-collection-company-use-tng              t)
  (evil-collection-term-sync-state-and-mode-p   t)
  (evil-collection-setup-minibuffer             t)
  (evil-collection-setup-debugger-keys          t)
  (evil-collection-company-use-tng            nil)
  :config
  (evil-collection-init)
  (general-define-key
   :states  '(normal motion)
   :keymaps 'evil-ex-completion-map
   "q"      #'abort-recursive-edit))

;; Evil Snipe, move the cursor more precisely!
(use-package evil-snipe
  :custom
  (evil-snipe-scope 'buffer)
  :config
  (evil-snipe-mode          1)
  (evil-snipe-override-mode 1)
  :general
  ;; Switch , with ; in snipe movements
  (general-define-key
   :states 'motion
   :keymaps 'evil-snipe-override-local-mode-map
   "," '(:ignore t)
   ";" '(:ignore t)
   ";" #'evil-snipe-repeat-reverse
   "," #'evil-snipe-repeat)

  (general-define-key
   :keymaps 'evil-snipe-parent-transient-map
   "," '(:ignore t)
   ";" '(:ignore t)
   ";" #'evil-snipe-repeat-reverse
   "," #'evil-snipe-repeat))

;; Evil-surround
(use-package evil-surround
  :config
  ;; `evil-surround-pairs-alist' is useful for definining new pairs
  (global-evil-surround-mode 1))

;; Evil commenter
(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; FIXME increment/decrement numbers, using `HYDRA'
;; (use-package evil-numbers
;;   :bind
;;   (("C-c +" . 'evil-numbers/inc-at-pt)
;;    ("C-c -" . 'evil-numbers/dec-at-pt)))

;; Additional matching on pairs, using %
(use-package evil-matchit
 :config
 (global-evil-matchit-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CORE PRESENTATION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call remove trailing whitespaces every time a buffer is saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; When saving a file that starts with `#!', make it executable.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Don't assume that sentences should have two spaces after periods. This ain't a typewriter.
(setq sentence-end-double-space nil)

;; Default tabs are not used for indentation
(setq-default indent-tabs-mode nil)

;; Default tab width
(setq-default tab-width 4)

;; Regions change face when selected..
(transient-mark-mode t)

;; If some text is selected, and you type some text, delete the selected text and start inserting your typed text.
(delete-selection-mode t)

;; If you save a file that doesn't end with a newline,automatically append one.
(setq require-final-newline t)

;; Get that ugly thing out of my sight
(when window-system
  (progn
    (tool-bar-mode -1)
    (menu-bar-mode -1)))

;; Sent font FIXME nicer fonts please
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-face-attribute 'default nil :font "Ubuntu Mono" :height 120)
(when (display-graphic-p)
  (set-fontset-font t 'latin "Ubuntu Mono"))

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

;; Window history.
(use-package winner
  :config
  (winner-mode)
  (general-define-key
   :states   '(normal motion)
   :keymaps  'winner-mode-map
             "<s-left>"  #'winner-undo
             "<s-right>" #'winner-redo))

;; Variation on one theme.
(use-package doom-themes
  :custom
  (doom-one-brighter-comments t)
  (doom-one-comment-bg nil)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  ;; The default region color can be a bit hard to see when other faces are active. Let's make it pop more.
  (set-face-attribute 'region nil
                      :background (doom-darken 'blue 0.55)
                      :inherit 'region))

;; Display battery info
(use-package battery
  :hook
  ;; FIXME Make it laptop specific
  ;; FIXME Further customize?
  '((after-init . display-battery-mode)))

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
    :states  '(normal visual motion)
    :keymaps 'override
    "h k M"  #'which-key-show-full-major-mode
    "h k m"  #'which-key-show-minor-mode-keymap)
  (which-key-mode))

;; Highlight indentation
;; FIXME activate for indent scope languages
;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides)

;; Never loose the cursor again!
(use-package beacon
  :custom
  (beacon-color (doom-darken 'blue 0.15))
  :config
  ;; FIXME Customize!
  ;; (setq beacon-blink-delay 0.1)
  ;; (setq beacon-blink-duration 0.25)
  ;; (setq beacon-size 25)
  (general-define-key
   :states '(normal motion)
   "S-SPC" #'beacon-blink)
  (beacon-mode))

;; Dashboard splash screen
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  ;; FIXME create a `my/dashboard-goto' interactive function.
  :custom
  (initial-buffer-choice (get-buffer "*dashboard*"))
  ;; FIXME Further customization here plz
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (show-week-agenda-p t)
  (dashboard-items '((recents   . 5)
                     (bookmarks . 5)
                     (projects  . 5)
                     (agenda    . 5)))
  (dashboard-show-shortcuts t))

;; FIXME Customize
;; All the icons (M-x all-the-icons-install-fonts)
(use-package all-the-icons)

;; ivy-rich (Fancy ivy) FIXME Customize
(use-package ivy-rich
  :config
  ;; All the icons integration (Mostly an example).
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))

  ;; FIXME CUSTOMIZE
  (defun ivy-rich-file-icon (candidate)
    (let ((icon (if (directory-name-p candidate)
                    (all-the-icons-icon-for-dir candidate)
                  (all-the-icons-icon-for-file candidate))))
      (if (symbolp icon)
          (all-the-icons-icon-for-file "unknown")
        icon)))

  (setq ivy-rich-display-transformers-list
        '(
          ivy-switch-buffer
          (:columns
           ;; FIXME Any way to "justify columns?"
           ((ivy-rich-switch-buffer-icon (:width 5))
            (ivy-switch-buffer-transformer
             (:width 50))
            (ivy-rich-switch-buffer-size
             (:width 7))
            (ivy-rich-switch-buffer-indicators
             (:width 3 :face error :align right))
            (ivy-rich-switch-buffer-major-mode
             (:width 12 :face warning))
            (ivy-rich-switch-buffer-project
             (:width 15 :face success))
            (ivy-rich-switch-buffer-path
             (:width
              (lambda
                (x)
                (ivy-rich-switch-buffer-shorten-path x
                                                     (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda
             (cand)
             (get-buffer cand)))
          counsel-find-file
          (:columns
           ((ivy-rich-file-icon (:width 5))
            (ivy-read-file-transformer (:width 30))
            (ivy-rich-counsel-find-file-truename
             (:face font-lock-doc-face))))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer
             (:width 40))
            (ivy-rich-counsel-variable-docstring
             (:face font-lock-doc-face))))
          counsel-recentf
          (:columns
           ((ivy-rich-file-icon (:width 5))
            (ivy-rich-candidate
             (:width 0.8))
            (ivy-rich-file-last-modified-time
             (:face font-lock-comment-face))))
          package-install
          (:columns
           ((ivy-rich-candidate
             (:width 30))
            (ivy-rich-package-version
             (:width 16 :face font-lock-comment-face))
            (ivy-rich-package-archive-summary
             (:width 7 :face font-lock-builtin-face))
            (ivy-rich-package-install-summary
             (:face font-lock-doc-face))))))

  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (ivy-rich-mode +1))


;; FIXME modules should be loaded in some other way, maybe through env variables?

;; editor fixme rename
(load-config "my-editor")

;; look and feel fixme
(load-config "generalconf")

;; navigation preferences
;; (load-config "navigation")

;;; programming languages / super modes
;; org
(load-config "my-org")

;; vimscript + vimrc syntax highlight
(load-config "vim")

;; elisp
(load-config "elisp")

;; haskell
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
