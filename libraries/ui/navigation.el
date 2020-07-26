;;; -*- lexical-binding:t -*-

;; Typing yes/no is obnoxious when y/n will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ask if you're sure that you want to close Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Window history.
(use-package winner
  :defer t
  :config
  (winner-mode)
  (general-define-key
   :states   '(normal motion)
   :keymaps  'winner-mode-map
             "<s-left>"  #'winner-undo
             "<s-right>" #'winner-redo))

;; TODO Research ace-window

;; Index the buffer code.
(use-package imenu
  :straight
  (:type built-in)
  :custom
  (use-package-enable-imenu-support t))

;; Nice looking index menu!
(use-package imenu-list
  :commands imenu-list-smart-toggle
  :custom
  (imenu-list-auto-resize t)
  (imenu-list-focus-after-activation t)
  (imenu-list-idle-update-delay 0.1)
  :general
  (dan/leader :states '(normal motion)
    "f i l"  #'imenu-list-smart-toggle)
  (:keymaps 'imenu-list-major-mode-map :states 'normal
            "RET" 'imenu-list-goto-entry
            "TAB" 'hs-toggle-hiding
            "d"   'imenu-list-display-entry
            "gr"  'imenu-list-refresh
            "q"   'imenu-list-quit-window))

;; Index that code, but everywhere!
(use-package imenu-anywhere
  :after ivy
  :commands ivy-imenu-anywhere
  :general
  (dan/leader :states '(normal motion)
    "f i i" #'ivy-imenu-anywhere))
