;;; -*- lexical-binding:t -*-

;; Better pdf viewing experience
(use-package pdf-tools
  :commands pdf-view-mode
  :mode ("\\.pdf\\'" . #'pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations t)
  (pdf-view-use-imagemagick nil)
  (pdf-view-use-scaling t)
  (pdf-view-resize-factor  1.125)
  :config

  (defun evil-collection-pdf-view-next-line-or-next-page (&optional count)
    "'evil' wrapper include a count argument to `pdf-view-next-line-or-next-page'"
    (interactive "P")
    (if count
        (dotimes (_ count nil)
          (pdf-view-next-line-or-next-page 1))
      (pdf-view-next-line-or-next-page 1)))

  (defun evil-collection-pdf-view-previous-line-or-previous-page (&optional count)
    "'evil' wrapper include a count argument to `pdf-view-previous-line-or-previous-page'"
    (interactive "P")
    (if count
        (dotimes (_ count nil)
          (pdf-view-previous-line-or-previous-page 1))
      (pdf-view-previous-line-or-previous-page 1)))

  (defun evil-collection-pdf-view-goto-page (&optional page)
    "`evil' wrapper around `pdf-view-last-page'."
    (interactive "P")
    (if page
        (pdf-view-goto-page page)
      (let ((hscroll (window-hscroll)))
        (pdf-view-last-page)
        (image-eob)
        (image-set-window-hscroll hscroll))))

  (defun evil-collection-pdf-view-goto-first-page (&optional page)
    "`evil' wrapper around `pdf-view-first-page'."
    (interactive "P")
    (if page
        (pdf-view-goto-page page)
      (let ((hscroll (window-hscroll)))
        (pdf-view-first-page)
        (image-bob)
        (image-set-window-hscroll hscroll))))

  ;; Viewing
  (dan/inhibit-evil-insert-state 'pdf-view-mode-map)
  (evil-set-initial-state 'pdf-view-mode 'normal)

  ;; Searching
  (dan/inhibit-evil-insert-state 'pdf-outline-buffer-mode-map)
  (evil-set-initial-state 'pdf-outline-buffer-mode 'normal)

  ;; Occur
  (dan/inhibit-evil-insert-state 'pdf-occur-buffer-mode-map)
  (evil-set-initial-state 'pdf-occur-buffer-mode 'normal)

  (pdf-loader-install :no-query)
  :general
  (:keymaps 'pdf-view-mode-map :states 'normal
            ;; motion
            "RET" 'image-next-line
            "j" 'evil-collection-pdf-view-next-line-or-next-page
            "k" 'evil-collection-pdf-view-previous-line-or-previous-page
            "SPC" 'pdf-view-scroll-up-or-next-page
            "S-SPC" 'pdf-view-scroll-down-or-previous-page
            "<delete>" 'pdf-view-scroll-down-or-previous-page
            "C-f" 'pdf-view-scroll-up-or-next-page
            "C-b" 'pdf-view-scroll-down-or-previous-page
            "]]" 'pdf-view-next-page-command
            "[[" 'pdf-view-previous-page-command
            "C-j" 'pdf-view-next-page-command
            "C-k" 'pdf-view-previous-page-command
            "gj" 'pdf-view-next-page-command
            "gk" 'pdf-view-previous-page-command
            "<next>" 'forward-page
            "<prior>" 'backward-page
            "<down>" 'evil-collection-pdf-view-next-line-or-next-page
            "<up>" 'evil-collection-pdf-view-previous-line-or-previous-page
            "gg" 'evil-collection-pdf-view-goto-first-page
            "G" 'evil-collection-pdf-view-goto-page

            ;; mark
            "'" 'pdf-view-jump-to-register
            "m" 'pdf-view-position-to-register

            ;; zoom
            "+" 'pdf-view-enlarge
            "zi" 'pdf-view-enlarge
            "=" 'pdf-view-enlarge
            "-" 'pdf-view-shrink
            "zo" 'pdf-view-shrink
            "0" 'pdf-view-scale-reset
            "z0" 'pdf-view-scale-reset
            "f" 'pdf-links-isearch-link
            "F" 'pdf-links-action-perform
            "h" 'image-backward-hscroll
            "^" 'image-bol
            "$" 'image-eol
            "l" 'image-forward-hscroll
            "H" 'pdf-view-fit-height-to-window ; evil-image has "H"
            "P" 'pdf-view-fit-page-to-window
            "W" 'pdf-view-fit-width-to-window ; evil-image has "W"

            ;; refresh
            "gr" 'revert-buffer
            "<C-down-mouse-1>" 'pdf-view-mouse-extend-region
            "<M-down-mouse-1>" 'pdf-view-mouse-set-region-rectangle
            "<down-mouse-1>"  'pdf-view-mouse-set-region
            "C-c C-c" 'docview-mode
            "C-c <tab>" 'pdf-view-extract-region-image
            "sb" 'pdf-view-set-slice-from-bounding-box
            "sm" 'pdf-view-set-slice-using-mouse
            "sr" 'pdf-view-reset-slice

            ;; goto
            "gl" 'pdf-view-goto-label

            ;; search
            "M-s o" 'pdf-occur
            "/" 'isearch-forward
            "?" 'isearch-backward
            "n" 'isearch-repeat-forward
            "N" 'isearch-repeat-backward
            "zd" 'pdf-view-dark-minor-mode
            "zm" 'pdf-view-midnight-minor-mode
            "zp" 'pdf-view-printer-minor-mode
            "o" 'pdf-outline

            ;; quit
            "q" 'quit-window
            "Q" 'kill-this-buffer
            "ZQ" 'kill-this-buffer
            "ZZ" 'quit-window
            "C-d" 'pdf-view-scroll-up-or-next-page
            "C-u" 'pdf-view-scroll-down-or-previous-page)
  (:keymaps 'pdf-view-mode-map :states 'visual
            "y" 'pdf-view-kill-ring-save)
  (:keymaps 'pdf-outline-buffer-mode-map :states 'normal
            ;; open
            "RET" 'pdf-outline-follow-link-and-quit
            "S-<return>" 'pdf-outline-follow-link
            "M-<return>" 'pdf-outline-display-link
            "go" 'pdf-outline-follow-link
            "." 'pdf-outline-move-to-current-page
            "SPC" 'pdf-outline-select-pdf-window
            "G" 'pdf-outline-end-of-buffer
            "^" 'pdf-outline-up-heading
            "<" 'pdf-outline-up-heading
            "zf" 'pdf-outline-follow-mode
            "<tab>" 'outline-toggle-children
            "<backtab>" 'pdf-outline-toggle-subtree

            ;; quit
            "C-w q" 'pdf-outline-quit-and-kill
            "q" 'quit-window
            "ZQ" 'quit-window
            "ZZ" 'pdf-outline-quit-and-kill)
  (:keymaps 'pdf-occur-buffer-mode-map :states 'normal
     ;; open
     "RET" 'pdf-occur-goto-occurrence
     "S-<return>" 'pdf-occur-view-occurrence
     "SPC" 'pdf-occur-view-occurrence
     "gd" 'pdf-occur-goto-occurrence
     "gD" 'pdf-occur-view-occurrence
     "A" 'pdf-occur-tablist-gather-documents
     "D" 'pdf-occur-tablist-do-delete
     ;; sort
     "o" 'tabulated-list-sort
     "O" 'tablist-sort

     ;; refresh
     "G" 'tablist-revert
     "K" 'pdf-occur-abort-search

     ;; mark
     "*m" 'tablist-mark-forward
     "m" 'tablist-mark-forward
     "~" 'tablist-toggle-marks
     "u" 'tablist-unmark-forward
     "U" 'tablist-unmark-all-marks
     "*!" 'tablist-unmark-all-marks
     "*c" 'tablist-change-marks
     "*n" 'tablist-mark-items-numeric
     "*r" 'tablist-mark-items-regexp
     "%"  'tablist-mark-items-regexp
     "a" 'tablist-flag-forward

     ;; "f" 'tablist-find-entry
     "r" 'pdf-occur-revert-buffer-with-args
     "d" 'tablist-do-kill-lines
     "x" 'pdf-occur-tablist-do-flagged-delete
     "<delete>" 'tablist-unmark-backward
     "S-SPC" 'scroll-down-command
     "<backtab>" 'tablist-backward-column
     "C-c C-e" 'tablist-export-csv
     [remap evil-first-non-blank] 'tablist-move-to-major-column
     [remap evil-next-line] 'tablist-next-line
     [remap evil-previous-line] 'tablist-previous-line

     ;; filter
     "/!" 'tablist-negate-filter
     "//" 'tablist-display-filter
     "/=" 'tablist-push-equal-filter
     "/C" 'tablist-clear-filter
     "/D" 'tablist-delete-named-filter
     "/a" 'tablist-push-named-filter
     "/d" 'tablist-deconstruct-named-filter
     "/e" 'tablist-edit-filter
     "/n" 'tablist-push-numeric-filter
     "/p" 'tablist-pop-filter
     "/r" 'tablist-push-regexp-filter
     "/s" 'tablist-name-current-filter
     "/t" 'tablist-toggle-first-filter-logic
     "/z" 'tablist-suspend-filter

     ;; quit
     "q" 'tablist-quit
     "ZQ" 'tablist-quit
     "ZZ" 'tablist-quit))
