;;;;;;;;;;;;;;;;;;
;;; COMPLETION ;;;
;;;;;;;;;;;;;;;;;;

;; TODO https://github.com/oantolin/orderless

;; General search engine. FIXME Customize
(use-package ivy
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  ;; Define the optimal height of the ivy buffer.
  (ivy-height-alist
   '(((t lambda (_caller) (/ (window-height) 4)))))
  (ivy-height 15)
  :config
  (ivy-mode +1)
  :general
  (:states '(motion normal)
           "<s-up>"   #'ivy-push-view
           "<s-down>" #'ivy-switch-view)
  (dan/leader
    :states   '(normal motion)
    :keymaps  'override
    "b b"      #'switch-to-buffer
    "i"        '(:ignore t :wk "[i]vy")
    "i r"      #'ivy-resume))

;; Hydra powered ivy!
(use-package ivy-hydra
  :custom
  (ivy-read-action-function 'ivy-hydra-read-action)
  :config
  (general-define-key :keymaps 'ivy-mode-map "C-c C-h" #'hydra-ivy/body))

;; Extra functions, powered by ivy.
(use-package counsel
  :general
  (dan/leader
    :states   '(normal motion)
    :keymaps  'override
    "b B"     #'counsel-switch-buffer
    "f f"     #'counsel-find-file
    "f r"     #'counsel-recentf
    "f l"     #'counsel-find-library
    "f F"     #'counsel-faces)
  (:keymaps
   'global-map [remap execute-extended-command] 'counsel-M-x))

;; Added M-x heuristics.
(use-package amx
  :custom
  (amx-backend 'ivy)
  (amx-save-file (concat dan/cache-dir "amx-items")))

;; Improve the default searching text functionality.
(use-package swiper
  :config
  (general-define-key :keymaps 'evil-motion-state-map
                      [remap evil-ex-search-forward]       #'swiper
                      [remap evil-ex-search-backward]      #'swiper-all
                      [remap evil-ex-search-word-forward]  #'swiper-thing-at-point
                      [remap evil-ex-search-word-backward] #'swiper-all-thing-at-point))

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
  :hook
  '(after-init . global-company-mode)
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

;; (use-package iedit)

;; (use-package evil-iedit-state)
