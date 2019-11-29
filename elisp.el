;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding: t -*-

;;; Code:
(use-package elisp-mode
  :after helpful
  :ensure nil
  :bind
  ("C-c C-d" . #'helpful-at-point))
  

(provide 'elisp)
;;; elisp.el ends here
