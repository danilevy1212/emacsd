;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;;; General navigation accross files and buffers

;;; Code:
;; FIXME ALL OF THIS; PART OF CORE.EL


;; TODO https://github.com/asok/all-the-icons-ivy?

;; Replace emacs internal search functions with friendlier ones and add some new ones
;; (use-package counsel
;;   :after ivy
;;   :general

;;   :config
;;   (counsel-mode +1)
;;   :bind*
;;   (("M-s r"   . 'ivy-resume)
;;    ("<f1> l"  . 'counsel-find-library)
;;    ("<f2> i"  . 'counsel-info-lookup-symbol)
;;    ("<f2> u"  . 'counsel-unicode-char)
;;    ("M-s g"   . 'counsel-git)
;;    ("M-y"     . 'counsel-yank-pop)
;;    ("M-s j"   . 'counsel-git-grep)
;;    ("M-y"     . 'counsel-yank-pop)
;;    ("M-o a"   . 'counsel-ag)
;;    ("C-x l"   . 'counsel-locate)
;;    :map ivy-minibuffer-map
;;    ("C-w"     . 'ivy-backward-kill-word)
;;    ("C-k"     . 'ivy-kill-line)
;;    ("C-j"     . 'ivy-immediate-done)
;;    ("RET"     . 'ivy-alt-done)))

;; TODO Use AMX?

;; Virtual workspaces FIXME replace with centaur tabs?
;; (use-package eyebrowse
;;   :config
;;   (eyebrowse-mode t)
;;   (evil-define-key '(normal motion) 'global (kbd "gt") 'eyebrowse-next-window-config)
;;   (evil-define-key '(normal motion) 'global (kbd "gT") 'eyebrowse-prev-window-config)
;;   (evil-define-key '(normal motion) 'global (kbd "zx") 'eyebrowse-last-window-config)
;;   (evil-define-key '(normal motion) 'global (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
;;   (evil-define-key '(normal motion) 'global (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
;;   (evil-define-key '(normal motion) 'global (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
;;   (evil-define-key '(normal motion) 'global (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
;;   (evil-define-key '(normal motion) 'global (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
;;   (evil-define-key '(normal motion) 'global (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
;;   (evil-define-key '(normal motion) 'global (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
;;   (evil-define-key '(normal motion) 'global (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
;;   (evil-define-key '(normal motion) 'global (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
;;   (evil-define-key '(normal motion) 'global (kbd "M-9") 'eyebrowse-switch-to-window-config-9)
;;   ;; (funcall-interactively 'eyebrowse-switch-to-window-config-0)
;;   ;; (funcall-interactively 'eyebrowse-rename-window-config 0 "agenda")
;;   :custom
;;   (eyebrowse-new-workspace t))

(provide 'navigation)
;;; navigation.el ends here
