;;; -*- lexical-binding:t -*-

;; Call remove trailing whitespaces every time a buffer is saved.
(add-hook 'before-save-hook
          #'delete-trailing-whitespace)

;; When saving a file that starts with `#!', make it executable.
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; Don't assume that sentences should have two spaces after periods. This ain't a typewriter.
(setq sentence-end-double-space nil)

;; Default tabs are not used for indentation
(setq-default indent-tabs-mode nil)

;; Default tab width
(setq-default tab-width 4)

;; Regions change face when selected..
(transient-mark-mode t)

;; If some text is selected, and you type some text, delete the selected text and start inserting your typed text.
(delete-selection-mode t)

;; Show only the right visual indicator.
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;; If you save a file that doesn't end with a newline,automatically append one.
(setq require-final-newline t)

;; Auto close text pairs.
(use-package elec-pair
  :straight
  (:type built-in)
  :config
  (electric-pair-mode))

;; Improve the default searching text functionality.
(use-package swiper
  :after ivy
  :defer 1
  :general
  (:keymaps 'evil-motion-state-map
                      [remap evil-ex-search-forward]       #'swiper
                      [remap evil-ex-search-backward]      #'swiper-all
                      [remap evil-ex-search-word-forward]  #'swiper-thing-at-point
                      [remap evil-ex-search-word-backward] #'swiper-all-thing-at-point))

;; TODO
;; (use-package iedit)

;; TODO
;; (use-package evil-iedit-state)
