;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-
;;; Code:

;; FIXME Use this as inpiration https://www.reddit.com/r/emacs/comments/6un4xo/developing_elisp/

;; FIXME with-simulated-input, make part of core utils?

(defun my-emacs-lisp-hook ()
  "Function to be run dureing the 'emacs-lisp-mode-hook'. Mainly configures keybindings for 'emacs-lisp-mode'."
  (let ((emacs-modes-maps '(emacs-lisp-mode-map lisp-interaction-mode-map)))
    (general-define-key
     :states 'normal
     :keymaps emacs-modes-maps
     "C-c C-d" #'helpful-at-point)
    (my-local-leader-def
      :keymaps emacs-modes-maps
      :states 'normal
      "h v" #'describe-variable
      "h f" #'describe-function
      "h d" #'helpful-at-point
      "f"   '(:ignore t :wk "[f]ind")
      "f d" #'xref-find-definitions
      "f D" #'xref-find-definitions-other-window
      ;; FIXME Use elisp-refs package instead!, make the search limited to current project directory, then all directories
      "f n" #'xref-find-references
      "r"   '(:ignore t :wk "[r]efactor")
      ;; FIXME use unread-command.events + xref-find-references?
      "r r" '((lambda () (interactive) (message "TODO")) :wk "rename symbol"))))

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-hook)
(add-hook 'lisp-interaction-mode-hook #'my-emacs-lisp-hook)

;;; elisp.el ends here
