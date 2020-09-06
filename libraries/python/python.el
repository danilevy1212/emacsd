;;; -*- lexical-binding:t -*-


;; Type correctness goodness
(use-package lsp-pyright
  :if (progn (fboundp #'lsp-deferred))
  :after pyvenv
  :init
  (defun dan/setup-python-lsp-pyright ()
    (interactive)
    (progn
      (require 'lsp-pyright)
      (lsp-deferred)))
  :hook
  '(pyvenv-mode . dan/setup-python-lsp-pyright))

;; Findme that virtual env!
(use-package pyvenv
  :config
  (defun dan/pyvenv-post-activation ()
      "Set correct Python interpreter after activation of venv."
    (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))
  (defun dan/pyvenv-post-deactivation ()
      "Set correct Python interpreter after deactivation of venv."
    (setq python-shell-interpreter "python3"))

  (add-hook 'pyvenv-post-activate-hooks #'dan/pyvenv-post-activation)
  (add-hook 'pyvenv-post-deactivate-hooks #'dan/pyvenv-post-deactivation)
  :commands pyvenv-mode
  :custom
  (pyvenv-tracking-ask-before-change t)
  :hook
  '(python-mode . pyvenv-mode))

;; Slow and easy.
(use-package python
  :mode
  (( "\\.py$" . python-mode))
  :straight
  (:type built-in))
