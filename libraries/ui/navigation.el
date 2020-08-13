;;; -*- lexical-binding:t -*-

;; Typing yes/no is obnoxious when y/n will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ask if you're sure that you want to close Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Window history.
(use-package winner
  :defer 1
  :config
  (winner-mode)
  :general
  (:states 'motion :keymaps 'winner-mode-map
           "<s-left>"  #'winner-undo
           "<s-right>" #'winner-redo))

;; A better other window.
(use-package ace-window
  :defer 1
  :custom
  (aw-minibuffer-flag t)
  :general
  (:states 'motion
           [remap evil-window-next] #'ace-window))

;; Monocle windows!
(use-package emacs
  :defer 1
  :commands dan/toggle-maximize-window
  :config
  (defun dan/toggle-maximize-window ()
    "Maximize current buffer"
    (interactive)
    (if (= 1 (length (window-list)))
        (jump-to-register '_)
      (progn
        (window-configuration-to-register '_)
        (delete-other-windows))))
  :general
  ;; NOTE Windower may make this redundant.
  (:states '(motion visual)
           "C-w m" '(dan/toggle-maximize-window :wk "[m]aximize/inimize")))

;; TODO https://gitlab.com/ambrevar/emacs-windower

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

;; TODO Customize!
;;; Workspaces (Emacs 27 "tabs")
;; (use-package tab-bar
;;   :config
;;   (setq tab-bar-close-button-show nil)
;;   (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
;;   (setq tab-bar-close-tab-select 'recent)
;;   (setq tab-bar-new-tab-choice t)
;;   (setq tab-bar-new-tab-to 'right)
;;   (setq tab-bar-position nil)
;;   (setq tab-bar-show nil)
;;   (setq tab-bar-tab-hints nil)
;;   (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)

;;   (tab-bar-mode -1)
;;   (tab-bar-history-mode -1)

;;   (defun prot/tab-bar-select-tab-dwim ()
;;     "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
;; If no other tab exists, create one and switch to it.  If there is
;; one other tab (so two in total) switch to it without further
;; questions.  Else use completion to select the tab to switch to."
;;     (interactive)
;;     (let ((tabs (mapcar (lambda (tab)
;;                           (alist-get 'name tab))
;;                         (tab-bar--tabs-recent))))
;;       (cond ((eq tabs nil)
;;              (tab-new))
;;             ((eq (length tabs) 1)
;;              (tab-next))
;;             (t
;;              (icomplete-vertical-do ()
;;                (tab-bar-switch-to-tab
;;                 (completing-read "Select tab: " tabs nil t)))))))

;;   :bind (("C-x t t" . prot/tab-bar-select-tab-dwim)
;;          ("s-t" . prot/tab-bar-select-tab-dwim)
;;          ("C-x t s" . tab-switcher)))
