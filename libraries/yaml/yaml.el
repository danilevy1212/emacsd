;;; -*- lexical-binding:t -*-

(defun dan/set-evil-shift-indentation ()
  "Set evil-shift indentation to the yaml default."
  (setq evil-shift-width yaml-indent-offset))

(use-package yaml-mode
  :mode ("\\.y[a]ml\\'")
  :hook '((yaml-mode . highlight-indent-guides-mode)
          (yaml-mode . dan/set-evil-shift-indentation-hook)))
