;;; -*- lexical-binding:t -*-

;;;;;;;;;;;;;;;;;;
;;; COMPLETION ;;;
;;;;;;;;;;;;;;;;;;

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

;; Extra functions, powered by ivy.
(use-package counsel
  :demand t
  :after ivy
  :commands (counsel-switch-buffer
             counsel-rg
             counsel-fzf
             counsel-find-file
             counsel-recentf
             counsel-find-library
             counsel-faces)
  :general
  (dan/leader
    :states   '(normal motion)
    :keymaps  'override
    "b B"     #'counsel-switch-buffer
    "f a"     #'counsel-rg
    "f f"     #'counsel-fzf ;; TODO https://github.com/muffinmad/emacs-find-file-rg/tree/ed556e092a92e325f335554ab193cef2d8fec009
    "f o"     #'counsel-find-file
    "f r"     #'counsel-recentf
    "f l"     #'counsel-find-library
    "f F"     #'counsel-faces)
  (:keymaps
   'global-map [remap execute-extended-command] #'counsel-M-x))

;; Look mom! It's like an IDE!
(use-package company
  :commands (company-mode dan/company-activate-for-coding)
  :init
  (progn
    (defconst dan/default-company-backends-coding
      '(company-capf company-files company-dabbrev-code)
      "Default backends to be used when coding with company.")
    (defun dan/company-activate-for-coding ()
      "Set up company for programming modes."
      (company-mode)
      (setq company-backends dan/default-company-backends-coding)))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0)
  (company-show-numbers t)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-width-grow-only t))

;; When you can't help but repeat yourself!
(use-package yasnippet
  :commands yas-minor-mode-on)


;; TODO company-yasnippet
