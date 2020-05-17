;;; package --- summary:
;;; Commentary:

;;; General global configuration of Emacs

;;; Code:
;;; -*- lexical-binding:t -*-

;; autoclose paranthesis
;; FIXME Part of core.el
(use-package smartparens
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap))
  (add-hook
   'after-init-hook  #'smartparens-global-mode))

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

;; FIXME Part of core.el
;; Modeline BUG Requires gitlab account through ssh
(use-package doom-modeline
  :custom
  (doom-modeline-modal-icon nil)
  :config
  (add-hook 'after-init-hook #'doom-modeline-mode))

;; FIXME Part of core.el
;; Rainbow Parentheses
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Better pdf view experience
(use-package pdf-tools
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  ;; :config
  ;; (pdf-tools-install)
  )

;; FIXME Part of core.el
;; Project management
(use-package projectile
  :custom
  (projectile-switch-project-action #'projectile-dired)
  (projectile-completion-system 'ivy)
  (projectile-sort-order 'recently-active)
  (projectile-cache-file (expand-file-name "projectile.cache" *my-cache-dir*))
  (projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" *my-cache-dir*))
  :config
  (projectile-mode +1)
  :general
  (my-leader-def
    :keymaps 'override
    :states  '(normal motion)
    ;; "p"     '(:prefix-map projectile-command-map :wk "[p]rojectile")
    "p"     '(:ignore t :wk "[p]rojectile")
    "p p"   '((lambda () (interactive) (projectile-switch-project)) :wk "[p]rojects")))


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
  ("M-s a" . #'ag-project))

;; better terminal emulation ~ special install akermu/emacs-libvterm
;; FIXME Use emacs application framework terminal?
(use-package vterm
  :straight
  (:no-byte-compile t :flavor melpa)
  :config

  ;; FIXME Work on this.
  ;; (setq display-buffer-alist
  ;;       '(("vterm"
  ;;          (display-buffer-in-side-window)
  ;;          (window-height . 0.25)
  ;;          (side . bottom)
  ;;          (slot . -1))))


  (my-leader-def
    :states '(normal motion)
    :keymaps 'override
    "t"      '(:ignore t :wk "[t]erminal")
    "t t"    #'vterm-other-window
    "t T"    #'vterm)
  ;;FIXME change `vterm-exit-functions' to use `kill-buffer-and-window' if more than one window with `count-windows'
  )

;; FIXME Part of core.el
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

;; Linting
(use-package flycheck
  :hook
  '(after-init . global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-display-errors-delay .3))

;; Use another frame to show error
(use-package flycheck-posframe
  :hook
  '(flycheck-mode . flycheck-posframe-mode))

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
  :general
  (:keymaps 'lsp-mode-map
	"C-c r" #'lsp-rename))

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
