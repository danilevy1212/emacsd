;; FIXME Reconsider this whole file, and refactor it for sure.
;;; Language Server Protocol support
(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental)
  (lsp-response-timeout 10)
  (lsp-eldoc-render-all nil)
  (lsp-eldoc-enable-hover t)
  (lsp-signature-render-all 'eldoc)
  :general
  (:keymaps 'lsp-mode-map
	"C-c r" #'lsp-rename))

;; UI tweaks
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-flycheck-enable nil)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-code-actions-prefix "")
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand)
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                         ,(face-foreground 'font-lock-string-face)
                         ,(face-foreground 'font-lock-constant-face)
                         ,(face-foreground 'font-lock-variable-name-face)))
  :preface
  (defun my/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1)))
  :general
  (:keymaps 'lsp-mode-map
            "C-c C-r" #'lsp-ui-peek-find-references
            "C-c C-j" #'lsp-ui-peek-find-definitions
            "C-c i"   #'lsp-ui-peek-find-implementation
            "C-c m"   #'lsp-ui-imenu
	        "C-c s"   #'lsp-ui-sideline-mode
	        "C-c d"   #'my/toggle-lsp-ui-doc))

;; Company backend
(use-package company-lsp
  :commands company-lsp
  :custom
  (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
  (company-lsp-async t)
  (company-lsp-enable-snippet t)
  (company-lsp-enable-recompletion t))

;; Debugger
;; (use-package dap-mode
;;   :config
;;   (dap-mode 1)
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1)
;;   (tooltip-mode 1)
;;   :hook
;;   (python-mode . (lambda () (require 'dap-python))))
