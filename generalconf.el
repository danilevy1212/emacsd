;;; package --- summary: -*- lexical-binding:t -*-
;;; Commentary:

;;; General global configuration of Emacs + basic startup setup

;;; Code:

;; For debugging, uncomment:
; (setf debug-on-error t)

(eval-when-compile
  ;; No splash screen on init
  (setq inhibit-splash-screen t)

  ;; Customize scratch buffer message
  (setq initial-scratch-message ";; Happy hacking ^_^\n\n")

  ;; Personal info
  (setq user-full-name "Daniel Levy Moreno"
        user-mail-address "daniellevymoreno@gmail.com")

  ;; Sent font
  (set-face-attribute 'default nil :font "Ubuntu Mono" :height 120)
  (set-fontset-font t 'latin "Ubuntu Mono")

  ;; Link to MELPA, org and gnu repository to download extra packages
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

  ;; bootstrap use-package
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)
    (package-install 'diminish)
    (package-install 'quelpa)
    (package-install 'bind-key))

  ;; :ensure is always set to t, thus all packages are checked to exist before loading
  (require 'use-package-ensure)
  (setq use-package-always-ensure t)

  ;; Disable warning and error messages at the time of loading packages
  (setq use-package-expand-minimally t))

;; Compile packages for faster loading times
(use-package auto-compile
 :config
 (auto-compile-on-load-mode)
 (auto-compile-on-save-mode))

;; Setup a special file for the customize interface
(setq custom-file "~/.emacs.d/custom/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;;; Sensible defaults (https://github.com/hrs/sensible-defaults.el/blob/master/sensible-defaults.el)
;; When opening a file, start searching at the user's home directory.
(setq default-directory "~/")

;; Call DELETE-TRAILING-WHITESPACE every time a buffer is saved.
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

;; When saving a file in a directory that doesn't exist, offer to (recursively) create the file's parent directories.
(add-hook 'before-save-hook
            (lambda ()
              (when buffer-file-name
                (let ((dir (file-name-directory buffer-file-name)))
                  (when (and (not (file-exists-p dir))
                             (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                    (make-directory dir t))))))

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

;; Visually indicate matching pairs of parentheses."
(show-paren-mode t)
(setq show-paren-delay 0.0)

;; When you perform a problematic operation, flash the screen instead of ringing the terminal bell.
(setq visible-bell t)

;; Set the default line length to 80."
(setq-default fill-column 80)

;; When middle-clicking the mouse to yank from the clipboard, insert the text where point is, not where the mouse cursor is.
(setq mouse-yank-at-point t)

;; Make yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; focus moves to help window
(setq help-window-select t)

;; Disable the menu bar.
(menu-bar-mode -1)

;; Disable the scrollbar.
(scroll-bar-mode -1)

;; Disable the toolbar.
(tool-bar-mode -1)

;; Describe in minibuffer what each key does while typing
(use-package which-key
  :custom
  (which-key-idle-delay 0.33)
  (which-key-allow-evil-operators t)
  (which-key-popup-type 'minibuffer)
  :config
  (which-key-mode))

;; autoclose paranthesis
(use-package smartparens
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))

;; show relative numbers on files
(use-package prog-mode
  :ensure nil
  :config
  (add-hook 'prog-mode-hook
            '(lambda ()
               (setq display-line-numbers 'relative))))

;; Don't show minor modes
(use-package minions
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

;; nyan-cat as the position in file indicator
(use-package nyan-mode
  :custom
  (nyan-animate-nyancat t)
  (nyan-cat-face-number 4)
  (nyan-wavy-trail t)
  :config
  (nyan-mode t))

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; All the icons (M-x all-the-icons-install-fonts)
(use-package all-the-icons)

;; Modeline (https://github.com/seagle0128/doom-modeline)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; Rainbow Parentheses
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Highlighting indentation
(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-auto-odd-face-perc 15)
  (highlight-indent-guides-auto-even-face-perc 15)
  (highlight-indent-guides-auto-character-face-perc 20)
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'stack)
  :hook
  (prog-mode . highlight-indent-guides-mode))

;; Better pdf view experience
(use-package pdf-tools
  :config
  (pdf-tools-install))

;; Project management
(use-package projectile
  :custom
  (projectile-switch-project-action #'projectile-dired)
  (projectile-completion-system 'ivy)
  (projectile-sort-order 'recently-active)
  :bind
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

;; Interactive wgrep buffer
(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; Use ag in backend
(use-package ag
  :custom
  (ag-highligh-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  :bind
  ("M-s a" . ag-project))

;; better terminal emulation ~ special install akermu/emacs-libvterm
(use-package vterm)

;; more helpful help screens
(use-package helpful
  :bind
  ("C-h k" . #'helpful-key)
  ("C-h F" . #'helpful-function)
  ("C-h C" . #'helpful-command)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

;; Auto complete
(use-package company
  :hook
  (after-init . global-company-mode)
  :commands company-complete-common company-manual-begin company-grab-line
  :bind*
  (("S-<return>" . company-complete-selection)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("<tab>" . company-complete-common-or-cycle)
   :map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next))
  :custom
  (company-begin-commands '(self-insert-command))
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (company-global-modes
   '(not erc-mode message-mode help-mode gud-mode eshell-mode))
  (company-backends '(company-capf company-dabbrev company-dabbrev-code))
  (company-frontends
   '(company-pseudo-tooltip-frontend
     company-echo-metadata-frontend)))

;; Pretty icons in company box
(use-package company-box
  :after company
  :hook
  (company-mode . company-box-mode)
  :init
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)
  (defun company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))
  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-fileicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
            (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
            (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
            (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
            (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
            (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
            (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
            (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
            (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
            (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
            (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
            (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
            (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
            (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))))))

;; Linting
(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-display-errors-delay .3))

;; Use another frame to show error
(use-package flycheck-posframe
  :after flycheck
  :hook
  (flycheck-mode . flycheck-posframe-mode))

;; Git porcelain
(use-package magit
  :custom
  (magit-auto-revert-mode nil)
  :bind
  ("M-g s" . magit-status))

;; Evil-like keybinds for magit
(use-package evil-magit
  :after magit)

;;; Language Server Protocol support
(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental)
  (lsp-response-timeout 10)
  (lsp-eldoc-render-all nil)
  (lsp-eldoc-enable-hover nil)
  :bind
  (:map lsp-mode-map
	("C-c r"   . lsp-rename)))

;; UI tweaks
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-flycheck-enable nil)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-code-actions-prefix "")
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand)
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
			 ,(face-foreground 'font-lock-string-face)
			 ,(face-foreground 'font-lock-constant-face)
			 ,(face-foreground 'font-lock-variable-name-face)))
  :preface
  (defun my/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
	(progn
	  (lsp-ui-doc-mode -1)
	  (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1)))
  :bind
  (:map lsp-mode-map
	("C-c C-r" . lsp-ui-peek-find-references)
	("C-c C-j" . lsp-ui-peek-find-definitions)
	("C-c i"   . lsp-ui-peek-find-implementation)
	("C-c m"   . lsp-ui-imenu)
	("C-c s"   . lsp-ui-sideline-mode)
	("C-c d"   . my/toggle-lsp-ui-doc)))

;; Company backend
(use-package company-lsp
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
  (company-lsp-async t)
  (company-lsp-enable-snippet t)
  (company-lsp-enable-recompletion t))

;; Debugger
(use-package dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  :hook
  (python-mode . (lambda () (require 'dap-python))))

;; Eldoc in a posframe
(use-package eldoc-box
  :hook
  (eldoc-mode . eldoc-box-hover-at-point-mode))

(provide 'generalconf)

;;; generalconf.el ends here
