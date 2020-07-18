;;; -*- lexical-binding:t -*-

;;;;;;;;;;;;;;;;;;
;;; COMPLETION ;;;
;;;;;;;;;;;;;;;;;;

;; TODO https://github.com/oantolin/orderless

;; Hydra powered ivy!
(use-package ivy-hydra
  :defer 1
  :custom
  (ivy-read-action-function 'ivy-hydra-read-action)
  :general
  (:keymaps 'ivy-mode-map "C-c C-h" #'hydra-ivy/body))

;; Added M-x heuristics.
(use-package amx
  :defer 1
  :custom
  (amx-backend 'ivy)
  (amx-save-file (concat dan/cache-dir "amx-items")))

;; (use-package iedit)

;; (use-package evil-iedit-state)
