;;; package --- summary:
;;; Commentary:

;;; General global configuration of Emacs

;;; Code:
;;; -*- lexical-binding:t -*-

;; Compile packages for faster loading times
(use-package auto-compile
 :config
 (auto-compile-on-load-mode)
 (auto-compile-on-save-mode))

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
(add-hook 'prog-mode-hook
          '(lambda ()
             (setq display-line-numbers 'relative)))

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

;; Modeline BUG Requires gitlab account through ssh
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
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  :config
  (pdf-tools-install))

;; Project management
(use-package projectile
  :custom
  (projectile-switch-project-action #'projectile-dired)
  (projectile-completion-system 'ivy)
  (projectile-sort-order 'recently-active)
  :bind-keymap
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
;; FIXME Use emacs application framework terminal?
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

;; Highlight TODO, FIXME, BUG words in comments FIXME Change to hl-mode
(use-package hl-todo
  :config
  (global-hl-todo-mode +1))

;; Auto complete
(use-package company
  :hook
  (after-init . global-company-mode)
  :commands company-complete-common company-manual-begin company-grab-line
  :bind (:map company-active-map
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

;; Linting
(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-display-errors-delay .3))

;; Use another frame to show error
(use-package flycheck-posframe
  :hook
  (flycheck-mode . flycheck-posframe-mode))

;; Git porcelain
(use-package magit
  :config
  (magit-auto-revert-mode +1))

;;; Language Server Protocol support
(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental)
  (lsp-response-timeout 10)
  (lsp-eldoc-render-all nil)
  (lsp-eldoc-enable-hover t)
  (lsp-signature-render-all 'eldoc)
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

(provide 'generalconf)

;;; generalconf.el ends here
