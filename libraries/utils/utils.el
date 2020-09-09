;;; -*- lexical-binding:t -*-

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; Customize away!
(use-package cus-edit
  :straight (:type built-in)
  :init
  (defun dan/load-custom-file-hook ()
    "Load the file containing the Customize interface options."
    (when (file-exists-p (symbol-value 'custom-file))
      (load custom-file nil nil)))
  :custom
  ;; Setup a special file for the customize interface.
  (custom-file (concat dan/cache-dir "custom.el"))
  ;; Show the names of entries as they are.
  (custom-unlispify-menu-entries nil)
  (custom-unlispify-tag-names nil)
  (custom-unlispify-remove-prefixes nil)
  :hook
  '(Custom-mode . dan/load-custom-file-hook)
  :config
  (evil-set-initial-state 'Custom-mode 'normal)
  :general
  (:keymaps 'custom-mode-map :states 'normal
            ;; motion
            "<tab>" 'widget-forward
            "S-<tab>" 'widget-backward
            "<backtab>" 'widget-backward
            "SPC" 'scroll-up-command
            "S-SPC" 'scroll-down-command
            "<delete>" 'scroll-down-command
            "RET" 'Custom-newline
            "]]" 'widget-forward
            "[[" 'widget-backward
            "C-j" 'widget-forward
            "C-k" 'widget-backward
            "gj" 'widget-forward
            "gk" 'widget-backward
            "^" 'Custom-goto-parent
            "C-o" 'Custom-goto-parent
            "<" 'Custom-goto-parent
            ;; quit
            "q" 'Custom-buffer-done
            "ZQ" 'evil-quit
            "ZZ" 'Custom-buffer-done))

;; Cache of recently visited files.
(use-package recentf
  :custom
  (recentf-save-file (concat dan/cache-dir "recentf"))
  (recentf-max-saved-items 30)
  (recentf-max-menu-items  30))

;; setup backups directory
(setq backup-directory-alist `((".*" . ,dan/backup-dir))
      make-backup-files t
      backup-by-copying t
      version-control   t
      kept-old-versions 2
      kept-new-versions 6
      auto-save-default t
      delete-old-versions t)

;; Bookmarks
(use-package bookmark
  :custom
  (bookmark-default-file (concat dan/cache-dir "bookmarks")))

;; Create directories of the buffer filename.
(add-hook 'before-save-hook
          'dan/create-directories-recursively)

;; More Helpful help screens
(use-package helpful
  :defer 1
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :general
  (:keymaps 'global-map
            "C-c C-d" #'helpful-at-point
            "C-h f"   #'helpful-callable
            "C-h v"   #'helpful-variable
            "C-h k"   #'helpful-key
            "C-h F"   #'helpful-function
            "C-h C"   #'helpful-command))

;; Example next to the help!
(use-package elisp-demos
  :after helpful
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; Project management
(use-package projectile
  :defer t
  :custom
  (projectile-switch-project-action #'projectile-dired)
  (projectile-completion-system 'ivy)
  (projectile-sort-order 'recently-active)
  (projectile-cache-file (expand-file-name "projectile.cache" dan/cache-dir))
  (projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" dan/cache-dir))
  :config
  (projectile-mode +1)
  :general
  (dan/leader
    :keymaps 'override
    :states  '(normal motion)
    "p"     '(:ignore t :wk "[p]rojectile")
    "p p"   '((lambda () (interactive) (projectile-switch-project)) :wk "[p]rojects")))

;; Linting
(use-package flycheck
  ;; NOTE We active the mode on each major mode on demand.
  :commands flycheck-mode
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-display-errors-delay .3)
  ;; FIXME Customize this
  ;; FIXME flycheck seems to wait until editing starts to check for errors, Can I change that?
  )

;; Use another frame to show linting errors
(use-package flycheck-posframe
  :if (display-graphic-p)
  :commands flycheck-posframe-mode
  :custom
  (flycheck-posframe-position 'window-top-right-corner)
  :config
  (flycheck-posframe-configure-pretty-defaults))

;;;###autoload
(defun dan/flycheck-maybe-in-posframe ()
    "Start flycheck in with flycheck-posframe frontend if available."
    (flycheck-mode)
    (when (fboundp #'flycheck-posframe-mode)
      (flycheck-posframe-mode)))

;; TODO https://github.com/nbfalcon/flycheck-projectile/tree/7d4ffda734785f87d6a74b6f34b2a4db234be114

;; TODO https://github.com/ieure/scratch-el Scratch buffers on demand!

;; TODO
;; https://blog.binchen.org/posts/how-to-use-expand-region-efficiently.html
;; (use-package expand-region)

;;; Language Server Protocol support
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (with-eval-after-load 'which-key
    ;; FIXME Weird bug here, gotta go into visual mode so the keybind shows in both visual an normal mode! Reproduce consistently and make issue?
    (dan/leader :states 'normal :keymaps 'lsp-mode-map
      "l"   '(:wk "[l]sp" :keymap lsp-command-map)
      "l s" '(:ignore t :wk "sessions")
      "l F" '(:ignore t :wk "folders")
      "l =" '(:ignore t :wk "formatting")
      "l T" '(:ignore t :wk "Toggle")
      "l g" '(:ignore t :wk "goto")
      "l r" '(:ignore t :wk "refactor")
      "l a" '(:ignore t :wk "action")
      "l h" '(:ignore t :wk "help")
      "l G" '(:ignore t :wk "peek")))
  :custom
  ;; NOTE Debugging
  ;; (lsp-server-trace t)
  ;; (lsp-log-io t)
  ;; (lsp-print-performance t)
  (lsp-enable-snippet t)
  (lsp-eldoc-render-all nil)
  (lsp-enable-xref t)
  (lsp-diagnostics-provider ':flycheck)
  (lsp-session-file  (concat dan/cache-dir ".lsp-session-v1"))
  (lsp-enable-indentation t)
  (lsp-enable-on-type-formatting t)
  (lsp-imenu-show-container-name t)
  (lsp-imenu-sort-methods '(kind position))
  (lsp-enable-file-watchers t)
  (lsp-keep-workspace-alive nil)
  (lsp-before-save-edits nil)
  (lsp-semantic-highlighting t )
  (lsp-enable-imenu t)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-enable-text-document-color t)
  (lsp-keymap-prefix  "C-c C-l"))


;; TODO
;; UI tweaks for lsp mode.
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-header t))

;; Ivy fueled symbol lookup.
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)
