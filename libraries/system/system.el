;;; -*- lexical-binding:t -*-

;;;;;;;;;;;;;;;;;;;;
;;; SYSTEM TOOLS ;;;
;;;;;;;;;;;;;;;;;;;;

;; Interactive wgrep buffer
(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; TODO Use emacs application framework terminal instead?
;; better terminal emulation ~ special install akermu/emacs-libvterm
(use-package vterm
  :commands (vterm vterm-other-window)
  ;; :config
  ;; FIXME Work on this. Also, `evil-collection' bindings
  ;; (setq display-buffer-alist
  ;;       '(("vterm"
  ;;          (display-buffer-in-side-window)
  ;;          (window-height . 0.25)
  ;;          (side . bottom)
  ;;          (slot . -1))))
  :general
  (dan/leader
    :states '(normal motion)
    :keymaps 'override
    "t"      '(:ignore t :wk "[t]erminal")
    "t t"    #'vterm-other-window
    "t T"    #'vterm)
  ;; FIXME change `vterm-exit-functions' to use `kill-buffer-and-window' if more than one window with `count-windows'
  )

;; Get me those delicious ENV variables.
(use-package exec-path-from-shell
  :defer 1
  :custom
  (exec-path-from-shell-variables '("PATH"
                                    "FZF_DEFAULT_COMMAND"
                                    "XDG_DATA_HOME"
                                    "CARGO_HOME"))
  (exec-path-from-shell-check-startup-files nil)
  :config
  ;; TODO Maybe more efficient to advice `counsel-fzf' with
  ;; `exec-path-from-shell-copy-env'
  (exec-path-from-shell-initialize))

;; Evil-like keybinds for magit
(use-package evil-magit
  :commands dan/init-evil-magit
  :init
  (defun dan/init-evil-magit ()
    (require 'evil-magit)))

;; Git porcelain
(use-package magit
  :commands magit-status
  :config
  (dan/init-evil-magit)
  ;; NOTE Please, don't ask me if it's safe!
  (add-to-list 'safe-local-variable-values
               '(magit-todos-rg-extra-args . ("--hidden")))
  (magit-auto-revert-mode +1)
  :general
  (:states '(normal motion) :keymaps  'magit-blame-mode-map
             "q" #'magit-blame-quit)
  (:states '(normal motion) :keymaps 'magit-blame-read-only-mode-map
           "q" #'magit-blame-quit)
  (dan/leader
    :states '(normal motion)
    :keymaps 'override
    "g"   '(:ignore t :wk "[g]it")
    "g s" #'magit-status))

;; No todo left behind!
(use-package magit-todos
  :hook
  '(magit-mode . magit-todos-mode)
  :custom
  (magit-todos-auto-group-items 'always)
  (magit-todos-keyword-suffix  (rx (optional "(" (1+ (not (any ")"))) ")") (optional ?: ) blank))
  :general
  (:keymaps '(magit-todos-section-map magit-todos-item-section-map)
            "jT" nil
            "jl" nil
            "j"  nil)
  (dan/leader
    :states '(normal motion)
    :keymaps 'override
    "i t"   #'ivy-magit-todos))

;; vim-gitgutter port
(use-package git-gutter
  :defer 2
  :custom
  (git-gutter:ask-p  nil)
  (git-gutter:update-interval 1)
  :custom-face
  (git-gutter:modified  ((t (:background "inherit"))))
  (git-gutter:unchanged ((t (:background "inherit"))))
  (git-gutter:deleted   ((t (:background "inherit"))))
  (git-gutter:added     ((t (:background "inherit"))))
  (git-gutter:separator ((t (:background "inherit"))))
  :config
  (global-git-gutter-mode)
  ;; g-based hunk previews
  :general
  (:states 'normal
   "g h p" 'git-gutter:popup-hunk
   ;; Jump to next/previous hunk
   "[ h" 'git-gutter:previous-hunk
   "] h" 'git-gutter:next-hunk
   ;; Stage current hunk
   "g h s" 'git-gutter:stage-hunk
   ;; Revert current hunk
   "g h u" 'git-gutter:revert-hunk
   ;; Mark current hunk
   "g h v" 'git-gutter:mark-hunk))

;; System directory browser.
(use-package dired
  :straight
  (:type built-in)
  :custom
  (dired-listing-switches "-alh")
  :general
  (:keymaps 'dired-mode-map
             :states 'normal
             "q" 'quit-window
             "j" 'dired-next-line
             "k" 'dired-previous-line
             [mouse-2] 'dired-mouse-find-file-other-window
             [follow-link] 'mouse-face
             ;; Commands to mark or flag certain categories of files
             "#" 'dired-flag-auto-save-files
             "." 'dired-clean-directory
             "~" 'dired-flag-backup-files
             ;; Upper case keys (except !) for operating on the marked files
             "A" 'dired-do-find-regexp
             "C" 'dired-do-copy
             "B" 'dired-do-byte-compile
             "D" 'dired-do-delete
             "gG" 'dired-do-chgrp
             "H" 'dired-do-hardlink
             "L" 'dired-do-load
             "M" 'dired-do-chmod
             "O" 'dired-do-chown
             "P" 'dired-do-print
             "Q" 'dired-do-find-regexp-and-replace
             "R" 'dired-do-rename
             "S" 'dired-do-symlink
             "T" 'dired-do-touch
             "X" 'dired-do-shell-command
             "Z" 'dired-do-compress
             "c" 'dired-do-compress-to
             "!" 'dired-do-shell-command
             "&" 'dired-do-async-shell-command
             ;; Comparison commands
             "=" 'dired-diff
             ;; Tree Dired commands
             "M-C-?" 'dired-unmark-all-files
             "M-C-d" 'dired-tree-down
             "M-C-u" 'dired-tree-up
             "M-C-n" 'dired-next-subdir
             "M-C-p" 'dired-prev-subdir
             ;; move to marked files
             "M-{" 'dired-prev-marked-file
             "M-}" 'dired-next-marked-file
             ;; Make all regexp commands share a `%' prefix:
             ;; We used to get to the submap via a symbol dired-regexp-prefix,
             ;; but that seems to serve little purpose, and copy-keymap
             ;; does a better job without it.
             "%" nil
             "%u" 'dired-upcase
             "%l" 'dired-downcase
             "%d" 'dired-flag-files-regexp
             "%g" 'dired-mark-files-containing-regexp
             "%m" 'dired-mark-files-regexp
             "%r" 'dired-do-rename-regexp
             "%C" 'dired-do-copy-regexp
             "%H" 'dired-do-hardlink-regexp
             "%R" 'dired-do-rename-regexp
             "%S" 'dired-do-symlink-regexp
             "%&" 'dired-flag-garbage-files
             ;; mark
             "*" nil
             "**" 'dired-mark-executables
             "*/" 'dired-mark-directories
             "*@" 'dired-mark-symlinks
             "*%" 'dired-mark-files-regexp
             "*(" 'dired-mark-sexp
             "*." 'dired-mark-extension
             "*O" 'dired-mark-omitted
             "*c" 'dired-change-marks
             "*s" 'dired-mark-subdir-files
             "*m" 'dired-mark
             "*u" 'dired-unmark
             "*?" 'dired-unmark-all-files
             "*!" 'dired-unmark-all-marks
             "U" 'dired-unmark-all-marks
             "* <delete>" 'dired-unmark-backward
             "* C-n" 'dired-next-marked-file
             "* C-p" 'dired-prev-marked-file
             "*t" 'dired-toggle-marks
             ;; Lower keys for commands not operating on all the marked files
             "a" 'dired-find-alternate-file
             "d" 'dired-flag-file-deletion
             "gf" 'dired-find-file
             "C-m" 'dired-find-file
             "gr" 'revert-buffer
             "i" 'dired-toggle-read-only
             "I" 'dired-maybe-insert-subdir
             "J" 'dired-goto-file
             "K" 'dired-do-kill-lines
             "r" 'dired-do-redisplay
             "m" 'dired-mark
             "t" 'dired-toggle-marks
             "u" 'dired-unmark                   ; also "*u"
             "W" 'browse-url-of-dired-file
             "x" 'dired-do-flagged-delete
             "gy" 'dired-show-file-type
             "Y" 'dired-copy-filename-as-kill
             "+" 'dired-create-directory
             ;; open
             "RET" 'dired-find-file
             "S-<return>" 'dired-find-file-other-window
             "M-RET" 'dired-display-file
             "gO" 'dired-find-file-other-window
             "go" 'dired-view-file
             ;; sort
             "o" 'dired-sort-toggle-or-edit
             ;; moving
             "gj" 'dired-next-dirline
             "gk" 'dired-prev-dirline
             "[[" 'dired-prev-dirline
             "]]" 'dired-next-dirline
             "<" 'dired-prev-dirline
             ">" 'dired-next-dirline
             "^" 'dired-up-directory
             "-" 'dired-up-directory
             " " 'dired-next-line
             [?\S-\ ] 'dired-previous-line
             [remap next-line] 'dired-next-line
             [remap previous-line] 'dired-previous-line
             ;; hiding
             "g$" 'dired-hide-subdir
             "M-$" 'dired-hide-all
             "(" 'dired-hide-details-mode
             ;; isearch
             "M-s a C-s"   'dired-do-isearch
             "M-s a M-C-s" 'dired-do-isearch-regexp
             "M-s f C-s"   'dired-isearch-filenames
             "M-s f M-C-s" 'dired-isearch-filenames-regexp
             ;; misc
             [remap read-only-mode] 'dired-toggle-read-only
             ;; `toggle-read-only' is an obsolete alias for `read-only-mode'
             [remap toggle-read-only] 'dired-toggle-read-only
             "g?" 'dired-summary
             "<delete>" 'dired-unmark-backward
             [remap undo] 'dired-undo
             [remap advertised-undo] 'dired-undo
             ;; thumbnail manipulation (image-dired)
             "C-t d" 'image-dired-display-thumbs
             "C-t t" 'image-dired-tag-files
             "C-t r" 'image-dired-delete-tag
             "C-t j" 'image-dired-jump-thumbnail-buffer
             "C-t i" 'image-dired-dired-display-image
             "C-t x" 'image-dired-dired-display-external
             "C-t a" 'image-dired-display-thumbs-append
             "C-t ." 'image-dired-display-thumb
             "C-t c" 'image-dired-dired-comment-files
             "C-t f" 'image-dired-mark-tagged-files
             "C-t C-t" 'image-dired-dired-toggle-marked-thumbs
             "C-t e" 'image-dired-dired-edit-comment-and-tags
             ;; encryption and decryption (epa-dired)
             ";d" 'epa-dired-do-decrypt
             ";v" 'epa-dired-do-verify
             ";s" 'epa-dired-do-sign
             ";e" 'epa-dired-do-encrypt))

;; Writable dired buffers.
(use-package wdired
  :after dired
  :straight
  (:type built-in))

;; Extra features for directories.
(use-package dired-x
  :after dired
  :straight
  (:type built-in))

(use-package dired-subtree
  :after dired
  :general
  (:keymaps 'dired-mode-map
             :states 'normal
             "TAB" 'dired-subtree-toggle
             "gh" 'dired-subtree-up
             "gl" 'dired-subtree-down
             "M-j" 'dired-subtree-next-sibling
             "M-k" 'dired-subtree-previous-sibling))

(use-package dired-filter
  :after dired
  :general
  (:keymaps 'dired-mode-map :states'normal
             "*"  'dired-filter-mark-map
             "g/" 'dired-filter-map))

;;narrow dired to match filter
(use-package dired-narrow
  :commands dired-narrow
  :general
  (:keymaps 'dired-mode-map :states 'normal
              "/"  'dired-narrow))

;;;;;;;;;;;;;;;;;;;;;;
;;; Keyboard Input ;;;
;;;;;;;;;;;;;;;;;;;;;;

;; MOZC integration in emacs. すごいですね！
(use-package mozc
  :commands mozc-mode
  :init
  (defun dan/toggle-mozc-mode ()
    "Toggle activation/deactivation of `mozc-mode'."
    (interactive)
    (let* ((active (mozc-mode))
           (msg (if active "Activated" "Deactivated")))
      (message "Mozc Mode %s" msg)))
  :general
  ;; NOTE Requires emacs_mozc_helper
  (:states '(normal insert)
           "C-x j" #'dan/toggle-mozc-mode)
  ;; Keychord to get out of insert mode, but for mozc
  (general-define-key :state 'insert :keymaps 'mozc-mode-map
                      "j" (general-key-dispatch 'self-insert-command
                            :timeout 0.25
                            "k" 'evil-normal-state))
  (general-define-key :state 'insert  :keymaps 'mozc-mode-map
                      "k" (general-key-dispatch 'self-insert-command
                            :timeout 0.25
                            "j" 'evil-normal-state)))
