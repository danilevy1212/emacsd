;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding: t -*-

;;; General global configuration of Emacs + basic startup setup

;;; Code:
(eval-when-compile
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
  (setq display-line-numbers 'relative))

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

;; Vim tabs
(use-package evil-tabs
  :after evil
  :config
  (global-evil-tabs-mode t))

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
;; (use-package company
;;   :ensure t
;;   :diminish company-mode
;;   :config
;;   (add-hook 'after-init-hook #'global-company-mode))

;; Flycheck
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'generalconf)
;;; generalconf.el ends here
