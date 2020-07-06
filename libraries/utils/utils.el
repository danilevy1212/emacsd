;;; -*- lexical-binding:t -*-

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

;; Customize away!
(use-package cus-edit
  :straight (:type built-in)
  :after evil
  :custom
  ;; Setup a special file for the customize interface.
  (custom-file (concat dan/cache-dir "custom"))
  :config
  (evil-set-initial-state 'Custom-mode 'normal)
  (when (file-exists-p custom-file)
    (load custom-file nil nil))
  (general-define-key
   :keymaps 'custom-mode-map :states 'normal
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
    ;; "p"     '(:prefix-map projectile-command-map :wk "[p]rojectile")
    "p"     '(:ignore t :wk "[p]rojectile")
    "p p"   '((lambda () (interactive) (projectile-switch-project)) :wk "[p]rojects")))

;; Linting
(use-package flycheck
  :hook
  '(after-init . global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-display-errors-delay .3)
  ;; FIXME Customize this
  ;; (flycheck-global-modes t)
  )


;; https://blog.binchen.org/posts/how-to-use-expand-region-efficiently.html
;; (use-package expand-region)
