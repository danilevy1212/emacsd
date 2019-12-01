;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;;; Code:
(use-package haskell-mode
  :config
  (set (make-local-variable 'company-backends)
       (append '((company-capf company-dabbrev-code)) company-backends)))

(provide 'haskell)
;;; haskell.el ends here
