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
    "f f"     #'counsel-fzf
    "f o"     #'counsel-find-file
    "f r"     #'counsel-recentf
    "f l"     #'counsel-find-library
    "f F"     #'counsel-faces)
  (:keymaps
   'global-map [remap execute-extended-command] #'counsel-M-x))

;; FIXME
;; Auto complete
;; (use-package company
;;   :hook
;;   '(after-init . global-company-mode)
;;   :commands company-complete-common company-manual-begin company-grab-line
;;   ;; :bind  (:map company-active-map
;;         ;;       ("C-n" . #'company-select-next)
;;         ;;       ("C-p" . #'company-select-previous)
;;         ;;       ("<tab>" . #'company-complete-common-or-cycle)
;;         ;;  :map company-search-map
;;         ;;       ("C-p" . #'company-select-previous)
;;         ;;       ("C-n" . #'company-select-next))
;;   :custom
;;   (company-begin-commands '(self-insert-command))
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.1)
;;   (company-show-numbers t)
;;   (company-tooltip-align-annotations 't)
;;   (company-global-modes
;;    '(not erc-mode message-mode help-mode gud-mode eshell-mode))
;;   (company-backends '(company-capf company-dabbrev company-dabbrev-code))
;;   (company-frontends
;;    '(company-pseudo-tooltip-frontend
;;      company-echo-metadata-frontend)))

;; FIXME Customize.
(use-package company
  :defer 1
  :config
  (global-company-mode)
  ;; :hook
  ;; '(after-init . global-company-mode)
  ;; :config
  ;; (setq company-clang-insert-arguments nil
  ;;       company-semantic-insert-arguments nil
  ;;       company-rtags-insert-arguments nil
  ;;       lsp-enable-snippet nil)
  ;; (advice-add #'eglot--snippet-expansion-fn :override #'ignore)
  ;; :custom
  ;; (company-require-match nil)
  ;; (company-frontends '(company-tng-frontend
  ;;                      company-pseudo-tooltip-frontend
  ;;                      company-echo-metadata-frontend))
  ;; :general
  ;; (:keymaps 'company-active-map
  ;;   "TAB"   'company-select-next
  ;;   "S-TAB" 'company-select-previous)
  )
