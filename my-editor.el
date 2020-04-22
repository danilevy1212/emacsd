;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;; Start evil mode
;;; Code:

;; FIXME Make this part of core.el


;; vim-like keybindings everywhere in emacs
(use-package evil-collection
  :config
  (evil-collection-init))

(use-package key-chord
  :custom
  (key-chord-two-keys-delay 0.25)
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state))

;; evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; FIXME increment/decrement numbers
;; (use-package evil-numbers
;;   :bind
;;   (("C-c +" . 'evil-numbers/inc-at-pt)
;;    ("C-c -" . 'evil-numbers/dec-at-pt)))

;; evil-commenter
(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; Additional matching on pairs, using %
(use-package evil-matchit
 :config
 (global-evil-matchit-mode 1))

;; Evil Snipe, move the cursor more precisely!
(use-package evil-snipe
  :custom
  (evil-snipe-scope 'buffer)
  :config
  (evil-snipe-mode          1)
  (evil-snipe-override-mode 1)
  )

;; Better sentence navigation
(use-package sentence-navigation
  :config
  (define-key evil-motion-state-map       ")"  'sentence-nav-evil-forward)
  (define-key evil-motion-state-map       "("  'sentence-nav-evil-backward)
  (define-key evil-motion-state-map       "g)" 'sentence-nav-evil-forward-end)
  (define-key evil-motion-state-map       "g(" 'sentence-nav-evil-backward-end)
  (define-key evil-outer-text-objects-map "s"  'sentence-nav-evil-a-sentence)
  (define-key evil-inner-text-objects-map "s"  'sentence-nav-evil-inner-sentence)
  :defer t)

;; This package provides gl and gL align operators
;; gl MOTION CHAR and right-align gL MOTION CHAR.
(use-package evil-lion
  :config
  (evil-lion-mode))

;; ex commands, which a vim user is likely to be familiar with
(use-package evil-expat
  :defer t)

;; Selected text in visual mode with * and # operators
(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

;; Folding
(use-package evil-vimish-fold
  :config
  (evil-vimish-fold-mode))

;; vim-gitgutter port
(use-package git-gutter
  :custom
  (git-gutter:update-interval 1)
  :config
  (global-git-gutter-mode t)
  ;; g-based hunk previews
  (evil-define-key 'normal 'global (kbd "g h p") 'git-gutter:popup-hunk)
  ;; Jump to next/previous hunk
  (evil-define-key 'normal 'global (kbd "[ h") 'git-gutter:previous-hunk)
  (evil-define-key 'normal 'global (kbd "] h") 'git-gutter:next-hunk)
  ;; Stage current hunk
  (evil-define-key 'normal 'global (kbd "g h s") 'git-gutter:stage-hunk)
  ;; Revert current hunk
  (evil-define-key 'normal 'global (kbd "g h u") 'git-gutter:revert-hunk)
  ;; Mark current hunk
  (evil-define-key 'normal 'global (kbd "g h v") 'git-gutter:mark-hunk))

(provide 'keybinding)
;;; keybinding.el ends here
