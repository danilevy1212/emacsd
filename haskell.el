;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;;; Code:
(use-package haskell-mode
  :config
  (set (make-local-variable 'company-backends)
       '(company-capf company-dabbrev-code)))

;; FIXME Use this for autocompletion an the rest of my needs.
;; https://github.com/haskell/haskell-ide-engine

(provide 'haskell)
;;; haskell.el ends here
