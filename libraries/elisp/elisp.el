;;; -*- lexical-binding:t -*-

;; Customize scratch buffer message
(setq initial-scratch-message (concat initial-scratch-message ";; Happy hacking ^_^\n\n"))

;; FIXME Use this as inpiration https://www.reddit.com/r/emacs/comments/6un4xo/developing_elisp/

;;; TODO Use
;;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/emacs-lisp/autoload.el#L217
;;; as inspiration to reduce the number of errors produces by checkdoc in
;;; libraries folder.

;;; TODO Add `lispyville' and `lispy'

;;; TODO  https://github.com/mhayashi1120/Emacs-erefactor/ Refactoring library

;; TODO if I ever get into other lisps, maybe good to create a lisp module and bring some stuff there
;; highlight quoted symbol
(use-package highlight-quoted
  :hook
  '((emacs-lisp-mode lisp-mode lisp-interaction-mode) . highlight-quoted-mode)
  :defer t)

;; Highlight sexp (Useful when evaluating from source)
(use-package highlight-sexp
  :hook
  '((emacs-lisp-mode lisp-mode lisp-interaction-mode) . highlight-sexp-mode)
  :defer t)

;; Jump to definitions
(use-package elisp-def
  :commands elisp-def)

;; Find references
(use-package elisp-refs
  ;; :config
  ;; FIXME Use thing-at-point to make variations of the functions, so I can search only
  ;; in current buffer, folder or project
  ;; Use the definition fro helpful-at-point for inpiration
  )

; Auto-compile emacslisp
(use-package auto-compile
  :commands auto-compile-on-save-mode
  :hook
  '(emacs-lisp-mode . auto-compile-on-save-mode))

(defun dan/emacs-lisp-hook ()
  "Function to be run dureing the `emacs-lisp-mode-hook'. Mainly configures keybindings for `emacs-lisp-mode'."
  (let ((emacs-modes-maps '(emacs-lisp-mode-map lisp-mode-map lisp-interaction-mode-map)))
    (dan/local-leader
      :keymaps emacs-modes-maps
      :states 'normal
      ;; FIXME Make some of these part of global too.
      "h v"   #'describe-variable
      "h f"   #'describe-function
      "h i"   #'info-lookup-symbol
      "h d"   #'helpful-at-point
      "f"     '(:ignore t :wk "[f]ind")
      "f d"   #'elisp-def
      "f D"   #'xref-find-definitions-other-window ;; FIXME
      "f r"   '(:ignore t :wk "[r]eferences")
      ;; FIXME make the search limited to current project directory, then all directories?
      "f r s" '((lambda (symbol)
                  (interactive
                   (list (symbol-at-point)))
                  (elisp-refs-symbol symbol
                                     (projectile-project-root))) :wk "[s]ymbol")
      "r"     '(:ignore t :wk "[r]efactor")
      ;; FIXME use unread-command.events + xref-find-references?
      ;;       probably will use idit, defined in core-utils
      "r r"   '((lambda () (interactive) (message "TODO")) :wk "rename symbol")
      "c"     '(:ignore t :wk "[c]ompile")
      "c c"   '((lambda () (interactive) (byte-compile-file buffer-file-name)) :wk "compile current file")
      "e"     '(:ignore t :which-key "[e]val")
      "e b"   'eval-buffer
      "e e"   'eval-last-sexp
      "e r"   'eval-region
      "e f"   'eval-defun
      "e p"   'eval-print-last-sexp
      "d"     '(:ignore t :which-key "[d]ebug"))))


;; FIXME adapt to my setup
;; (defun +emacs-lisp-reduce-flycheck-errors-in-emacs-config-h ()
;;   "Remove `emacs-lisp-checkdoc' checker and reduce `emacs-lisp' checker
;; verbosity when editing a file in `doom-private-dir' or `doom-emacs-dir'."
;;   (when (and (bound-and-true-p flycheck-mode)
;;              (eq major-mode 'emacs-lisp-mode)
;;              (or (not buffer-file-name)
;;                  (cl-loop for dir in (list doom-emacs-dir doom-private-dir)
;;                           if (file-in-directory-p buffer-file-name dir)
;;                           return t)))
;;     (add-to-list (make-local-variable 'flycheck-disabled-checkers)
;;                  'emacs-lisp-checkdoc)
;;     (set (make-local-variable 'flycheck-emacs-lisp-check-form)
;;          (concat "(progn "
;;                  (prin1-to-string
;;                   `(progn
;;                      (setq doom-modules ',doom-modules
;;                            doom-disabled-packages ',doom-disabled-packages)
;;                      (ignore-errors (load ,user-init-file t t))
;;                      (setq byte-compile-warnings
;;                            '(obsolete cl-functions
;;                              interactive-only make-local mapcar
;;                              suspicious constants))
;;                      (defmacro map! (&rest _))))
;;                  " "
;;                  (default-value 'flycheck-emacs-lisp-check-form)
;;                  ")"))))

(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook))
  (add-hook hook #'dan/emacs-lisp-hook))

;;; elisp.el ends here
