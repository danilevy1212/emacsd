;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-
;;; Code:

(use-package auctex-latexmk
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package reftex
  :defer t
  :custom
  (reftex-cite-prompt-optional-args t))

(use-package auto-dictionary
  :after flyspell
  :config
  (add-hook 'flyspell-mode-hook '(lambda () (auto-dictionary-mode 1))))

(use-package company-auctex)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(add-hook 'LaTeX-mode-hook '(lambda ()
                              (flyspell-mode t)
                              (reftex-mode t)
                              (set (make-local-variable 'company-backends)
                                   (append '((company-auctex-macros
                                              company-auctex-symbols
                                              company-auctex-environments)
                                             company-auctex-bibs
                                             company-auctex-labels)
                                           company-backends))
                              (setq TeX-source-correlate-mode t
                                    TeX-source-correlate-method 'synctex
                                    TeX-auto-save t
                                    TeX-parse-self t
                                    reftex-plug-into-AUCTeX t
                                    TeX-view-program-selection '((output-pdf "PDF Tools"))
                                    TeX-source-correlate-start-server t)
                              (setq-default TeX-engine 'luatex)))

(provide 'latex)
;;; latex.el ends here
