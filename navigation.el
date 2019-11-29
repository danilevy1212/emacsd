;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding: t -*-

;;; General navigation accross files and buffers

;;; Code:
;; General search engine
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  :config
  (ivy-mode 1))

;; Replace emacs internal search functions with friendlier ones and add some new ones
(use-package counsel
  :after ivy
  :config
  (counsel-mode 1)
  :bind*
  (("M-s r"   . 'ivy-resume)
   ("M-x"     . 'counsel-M-x)
   ("<f1> l"  . 'counsel-find-library)
   ("<f2> i"  . 'counsel-info-lookup-symbol)
   ("<f2> u"  . 'counsel-unicode-char)
   ("M-s g"   . 'counsel-git)
   ("M-y"     . 'counsel-yank-pop)
   ("M-s j"   . 'counsel-git-grep)
   ("M-y"     . 'counsel-yank-pop)
   ("M-o a"   . 'counsel-ag)
   ("C-x l"   . 'counsel-locate)
   ("C-M-r"   . 'counsel-recentf)
   :map ivy-minibuffer-map
   ("C-w"     . 'ivy-backward-kill-word)
   ("C-k"     . 'ivy-kill-line)
   ("C-j"     . 'ivy-immediate-done)
   ("RET"     . 'ivy-alt-done)))


(provide 'navigation)
;;; navigation.el ends here
