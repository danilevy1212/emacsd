;;; -*- lexical-binding:t -*-

;;; Major mode for all sorts of frontend file types.
(use-package web-mode
  :mode
  (("\\.html?\\'" . web-mode))
  (("\\.vue\\'"   . dan/vue-setup-hook))
  :init
  (defun dan/vue-setup-hook ()
    "Setup vue files configuration for web-mode"
    (progn
      (set (make-local-variable 'company-backends)
           (append '(company-capf company-dabbrev-code)))
      (when (fboundp #'lsp-deferred)
        (lsp-deferred))
      (when (fboundp #'prettier-js-mode)
        (prettier-js-mode +1))
      (when (fboundp #'dan/company-activate-for-coding)
        (dan/company-activate-for-coding))
      (web-mode)))
  :custom
  (web-mode-enable-engine-detection           t)
  (web-mode-attr-indent-offset                2)
  (web-mode-block-padding                     2)
  (web-mode-css-indent-offset                 2)
  (web-mode-code-indent-offset                2)
  (web-mode-comment-style                     2)
  (web-mode-enable-current-element-highlight  t)
  (web-mode-markup-indent-offset              2)
  (web-mode-style-padding                     0)
  (web-mode-script-padding                    0)
  ;; NOTE LSP settings.
  (lsp-vetur-use-workspace-dependencies       t)
  (lsp-vetur-experimental-template-interpolation-service t)
  (lsp-typescript-references-code-lens-enabled t))

;; Reformat those files!
(use-package prettier-js
  :commands (prettier-js-mode prettier-js)
  ;; :mode
  ;; (("\\.vue\\'"   . prettier-js-mode))
  )
