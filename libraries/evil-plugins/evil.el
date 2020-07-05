;;; -*- lexical-binding:t -*-

;;;;;;;;;;;;;;;;;;;;;
;;; VIM EMULATION ;;;
;;;;;;;;;;;;;;;;;;;;;

;; FIXME This mode is way too much bloat, keep it for reference, and only pick and choose what I like from it.
;; FIXME Translate the keybindings I like with general.
(use-package evil-collection
  :custom
  (evil-collection-mode-list '(ivy
                               info
                               occur
                               ;; dired
                               doc-view
                               (pdf pdf-view)
                               helpful
                               magit
                               minibuffer
                               dashboard
                               company
                               popup
                               help
                               helpful
                               vterm
                               wgrep))
  (evil-collection-outline-bind-tab-p           t)
  (evil-collection-company-use-tng              t)
  (evil-collection-term-sync-state-and-mode-p   t)
  (evil-collection-setup-minibuffer             t)
  (evil-collection-setup-debugger-keys          t)
  (evil-collection-company-use-tng            nil)
  :config
  (evil-collection-init)
  (general-define-key
   :states  '(normal motion)
   :keymaps 'evil-ex-completion-map
   "q"      #'abort-recursive-edit))

;; Evil Snipe, move the cursor more precisely!
(use-package evil-snipe
  :custom
  (evil-snipe-scope 'buffer)
  :config
  (evil-snipe-mode          1)
  (evil-snipe-override-mode 1)
  :general

  ;; FIXME Since using an american keyboard, maybe this is hugely unnecesary?
  ;; Switch , with ; in snipe movements
  (general-define-key
   :states 'motion
   :keymaps 'evil-snipe-override-local-mode-map
   "," '(:ignore t)
   ";" '(:ignore t)
   ";" #'evil-snipe-repeat-reverse
   "," #'evil-snipe-repeat)

  (general-define-key
   :keymaps 'evil-snipe-parent-transient-map
   "," '(:ignore t)
   ";" '(:ignore t)
   ";" #'evil-snipe-repeat-reverse
   "," #'evil-snipe-repeat))

;; Evil-surround
(use-package evil-surround
  :config
  ;; `evil-surround-pairs-alist' is useful for definining new pairs
  (global-evil-surround-mode 1))

;; Evil commenter
(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; Additional matching on pairs, using %
(use-package evil-matchit
 :config
 (global-evil-matchit-mode 1))

;; Better sentence navigation
(use-package sentence-navigation
  :general
  (:keymaps 'evil-motion-state-map
            ")"  'sentence-nav-evil-forward
            "("  'sentence-nav-evil-backward
            "g)" 'sentence-nav-evil-forward-end
            "g(" 'sentence-nav-evil-backward-end)
  (:keymaps 'evil-outer-text-objects-map
            "s"  'sentence-nav-evil-a-sentence)
  (:keymaps 'evil-inner-text-objects-map
            "s"  'sentence-nav-evil-inner-sentence))

;; This package provides gl and gL align operators
;; gl MOTION CHAR and right-align gL MOTION CHAR.
(use-package evil-lion
  :config
  (evil-lion-mode))

;; ex commands, which a vim user is likely to be familiar with
(use-package evil-expat
  :defer 1)

;; FIXME increment/decrement numbers, using `HYDRA'
;; (use-package evil-numbers
;;   :bind
;;   (("C-c +" . 'evil-numbers/inc-at-pt)
;;    ("C-c -" . 'evil-numbers/dec-at-pt)))
