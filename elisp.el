;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;;; Code:
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (evil-define-key 'normal 'local (kbd "C-c C-d") #'helpful-at-point)))

;;; elisp.el ends here
