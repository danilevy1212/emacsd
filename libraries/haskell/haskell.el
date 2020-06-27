;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;;; Code:
(use-package haskell-mode
  ;; :custom
  ;; FIXME Until I use stack for xmonad.
  ;; (flycheck-ghc-args '("-dynamic"))
  :config
  (set (make-local-variable 'company-backends)
       '(company-capf company-dabbrev-code)))

;; FIXME Use this for autocompletion.
;; https://chrisdone.github.io/intero/ NOTE No longer maintained!
;; https://github.com/horellana/company-ghci
;; https://github.com/jyp/dante
;; FIXME Alternative to intereo/date, cos I rather not have to use lsp if I don't need to.
;; https://github.com/haskell/haskell-ide-engine
;; https://github.com/digital-asset/ghcide

(provide 'haskell)
;;; haskell.el ends here
