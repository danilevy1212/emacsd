;;; package --- summary: -*- lexical-binding:t -*-
;;; Commentary:

;;; General global configuration of Emacs + basic startup setup

;;; Code:
(eval-when-compile
  ;; Personal info
  (setq user-full-name "Daniel Levy Moreno"
        user-mail-address "daniellevymoreno@gmail.com"
        calendar-latitude 40.41
        calendar-longitude -3.70
        calendar-location-name "Madrid, Madrid")

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
(setq custom-file "~/.emacs.d/.config/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Describe in minibuffer what each key does while typing
(use-package which-key
  :init
  (setq which-key-idle-delay 0.33
        which-key-allow-evil-operators t
        which-key-popup-type 'minibuffer)
  :config
  (which-key-mode))

;; Some more sensible defaults
(use-package better-defaults
  :custom
  (apropos-sort-by-scores t))

;; autoclose paranthesis
(use-package smartparens
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap))
  :defer)

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

;; Vim tabs FIXME: Substitute with eyebrowse
;; (use-package evil-tabs
;;   :after evil
;;   :config
;;   (global-evil-tabs-mode t))

;; Rainbow Parentheses
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Better pdf view experience
(use-package pdf-tools
  :config
  (pdf-tools-install))

;; Project management
(use-package projectile
  :diminish
  :bind
  ("M-o p" . counsel-projectile-switch-project)
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

;; Make yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; better terminal emulation ~ special install akermu/emacs-libvterm
(use-package vterm)

;; focus moves to help window
(setq help-window-select t)

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
  (after-init . company-mode)
  :commands company-complete-common company-manual-begin company-grab-line
  :bind
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
  (company-backends '(company-capf))
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
  (flycheck-display-errors-delay .3)
  :config
  ;; FIXME: Very ugly hack,open a issue about it?
  (defun flycheck-global-teardown (&optional ignore-local)
    "Teardown Flycheck in all buffers.

Completely clear the whole Flycheck state in all buffers, stop
all running checks, remove all temporary files, and empty all
variables of Flycheck.

Also remove global hooks.  (If optional argument IGNORE-LOCAL is
non-nil, then only do this and skip per-buffer teardown.)"
    (unless ignore-local
      (dolist (buffer (buffer-list))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when flycheck-mode
              (flycheck-teardown 'ignore-global))))))
    (remove-hook 'buffer-list-update-hook #'flycheck-handle-buffer-switch)))

;; Use another frame to show error
(use-package flycheck-posframe
  :after flycheck
  :hook
  (flycheck-mode . flycheck-posframe-mode))

(provide 'generalconf)

;;; generalconf.el ends here
