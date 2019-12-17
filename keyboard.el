;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;; Start evil mode
;;; Code:
(use-package evil
  :init
  (setq evil-want-keybinding                  nil
        evil-search-module                    'evil-search
        evil-vsplit-window-right              t
        evil-indent-convert-tabs              t
        evil-split-window-below               t
        evil-ex-search-vim-style-regexp       t
        evil-shift-round                      nil
        evil-want-C-u-scroll                  t)
  :config
  ;; Remove highlighted sections with ctrl + l
  (evil-define-key 'normal 'global (kbd "C-l") #'evil-ex-nohighlight)
  ;; Universal argument mapped to M-u globally
  (evil-define-key 'normal 'global (kbd "M-u") #'universal-argument)
  (evil-mode 1))

;; vim-like keybindings everywhere in emacs
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; evil-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; increment/decrement numbers
(use-package evil-numbers
  :bind*
  (("C-c +" . 'evil-numbers/inc-at-pt)
   ("C-c -" . 'evil-numbers/dec-at-pt)))

;; evil-commenter
(use-package evil-commentary
  :config
  (evil-commentary-mode))

;; Setting a leader key
(use-package evil-leader
  :after counsel
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "o a"   'org-agenda
    "o l"   'org-store-link
    "e"     'counsel-find-file
    "E"     'find-file-other-window
    "b"     'counsel-switch-buffer
    "B"     'counsel-switch-buffer-other-window
    "r"     'counsel-recentf
    "f"     'counsel-fzf ;; Requires "fzf"
    "C-k"   'kill-buffer
    "C-S-k" 'only-current-buffer
    "k"     'kill-current-buffer
    "K"     'kill-buffer-and-window
    "t"     'vterm-other-window
    "T"     'vterm)
  (global-evil-leader-mode))

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
  (evil-snipe-override-mode 1))

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
  :ensure t
  :config
  (evil-lion-mode))

;; ex commands, which a vim user is likely to be familiar with
(use-package evil-expat
  :defer t)

;; Selected text in visual mode with * and # operators
(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

;; Vim keys on org-mode
(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            '(lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Folding
(use-package evil-vimish-fold
  :config
  (evil-vimish-fold-mode))

;; vim-gitgutter port
(use-package git-gutter
  :after evil
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


(provide 'keyboard)
;;; keyboard.el ends here
