;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;;; Code:
(use-package yaml-mode
  :mode ("\\.y[a]ml\\'")
  :hook (yaml-mode . highlight-indent-guides-mode))

;;; yaml.el ends here
