;;; -*- lexical-binding:t -*-

;;; Static functional goodness!
(use-package haskell-mode
  :mode
  ("\\.hs\\'" . #'haskell-mode)
  ;; :custom
  ;; FIXME Until I use stack for xmonad.
  ;; (flycheck-ghc-args '("-dynamic"))
  )

;; FIXME This ain't working man...
(use-package company-ghci
  :init
  (defun dan/haskell-set-company-backends-hook ()
    "Hook to set the company-backends in `haskell-mode'"
    (set (make-local-variable 'company-backends)
         '(company-ghci company-capf company-dabbrev-code)))
  :hook
  '(haskell-mode . dan/haskell-set-company-backends-hook))


;; FIXME Use this for autocompletion.
;; https://chrisdone.github.io/intero/ NOTE No longer maintained!
;; https://github.com/horellana/company-ghci
;; https://github.com/jyp/dante
;; FIXME Alternative to intereo/date, cos I rather not have to use lsp if I don't need to.
;; https://github.com/haskell/haskell-ide-engine
;; https://github.com/digital-asset/ghcide
