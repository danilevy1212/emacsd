;;; -*- lexical-binding: t -*-

(use-package elisp-mode
  :after helpful
  :ensure nil
  :bind
  ("C-c C-d" . #'helpful-at-point))
  
