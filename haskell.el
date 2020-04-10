;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;;; Code:
(use-package haskell-mode
  :config
  (set (make-local-variable 'company-backends)
       '(company-capf company-dabbrev-code)))

(provide 'haskell)
;;; haskell.el ends here
