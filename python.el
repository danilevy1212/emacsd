;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;;; Code:

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(provide 'python)
;;; python.el ends here
