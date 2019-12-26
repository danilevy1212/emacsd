;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;;; Code:

;; (use-package elpy
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable)
;;   :custom
;;   (python-shell-interpreter "ipython")
;;   (python-shell-interpreter-args "-i --simple-prompt")
;;   :config
;;   ;; remove flymake
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(use-package python-mode
  :after ipython-shell-send
  :ensure nil
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  :bind
  ("C-c C-s" . ipython-shell-send-string)
  ("C-c C-c" . ipython-shell-send-buffer)
  ("C-c C-l" . ipython-shell-send-defun)
  ("C-M-x"   . ipython-shell-send-region))

(use-package ipython-shell-send)

(use-package lsp-python-ms
  :hook
  (python-mode . (lambda ()
		   (require 'lsp-python-ms)
		   (lsp-deferred)))
  :config
  (unless (file-exists-p lsp-python-ms-executable)
    ;; for executable of language server, if it's not symlinked on your PATH
    (setq lsp-python-ms-executable
	  "~/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer")))


(use-package pyvenv
  :hook
  (python-mode . pyvenv-mode))


(provide 'python)
;;; python.el ends here
