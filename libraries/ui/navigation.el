;;; -*- lexical-binding:t -*-

;; Typing yes/no is obnoxious when y/n will do
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Ask if you're sure that you want to close Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;; Window history.
(use-package winner
  :config
  (winner-mode)
  (general-define-key
   :states   '(normal motion)
   :keymaps  'winner-mode-map
             "<s-left>"  #'winner-undo
             "<s-right>" #'winner-redo))

;; TODO Research ace-window
