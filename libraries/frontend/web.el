;;; -*- lexical-binding:t -*-

;;; Major mode for all sorts of frontend file types.
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode))
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
  :hook (web-mode . (lambda ()
                      (set (make-local-variable 'company-backends)
                           (append '((company-lsp company-capf company-dabbrev-code)) '()))
                      (lsp-deferred))))
