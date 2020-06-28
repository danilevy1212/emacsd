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

;; Wrap those lines!
(global-visual-line-mode t)

;; Show only the right visual indicator.
(setq visual-line-fringe-indicators '(nil right-curly-arrow))

;; If you save a file that doesn't end with a newline,automatically append one.
(setq require-final-newline t)
