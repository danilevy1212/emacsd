;;; -*- lexical-binding:t -*-

;; RSS client
(use-package elfeed
  :commands elfeed
  :config
  (dan/inhibit-evil-insert-state 'elfeed-search-mode-map)
  (evil-set-initial-state 'elfeed-search-mode 'normal)
  (dan/inhibit-evil-insert-state 'elfeed-show-mode-map)
  (evil-set-initial-state 'elfeed-show-mode 'normal)
  :custom
  (elfeed-db-directory (concat dan/cloud-dir "elfeed/"))
  :general
  (:states 'normal :keymaps 'elfeed-search-mode-map
           ;; open
           "RET" 'elfeed-search-show-entry
           "S-<return>" 'elfeed-search-browse-url
           "go" 'elfeed-search-browse-url
           "y" 'elfeed-search-yank
           "SPC" 'scroll-up-command
           "S-SPC" 'scroll-down-command
           ;; filter
           "s" 'elfeed-search-live-filter
           "S" 'elfeed-search-set-filter
           ;; refresh
           "gR" 'elfeed-search-fetch
           "gr" 'elfeed-search-update--force
           ;; quit
           "q" 'quit-window
           "ZQ" 'quit-window
           "ZZ" 'quit-window)
  (:states '(normal visual) :keymaps 'elfeed-search-mode-map
           "+" 'elfeed-search-tag-all
           "-" 'elfeed-search-untag-all
           "U" 'elfeed-search-tag-all-unread
           "u" 'elfeed-search-untag-all-unread)
  (:states 'normal :keymaps 'elfeed-show-mode-map
           "S-<return>" 'elfeed-show-visit
           "go" 'elfeed-show-visit
           "SPC" 'scroll-up-command
           "S-SPC" 'scroll-down-command
           "<tab>" 'elfeed-show-next-link
           ;; filter
           "s" 'elfeed-show-new-live-search
           "+" 'elfeed-show-tag
           "-" 'elfeed-show-untag
           "A" 'elfeed-show-add-enclosure-to-playlist
           "P" 'elfeed-show-play-enclosure
           "d" 'elfeed-show-save-enclosure
           "]]" 'elfeed-show-next
           "[[" 'elfeed-show-prev
           "gj" 'elfeed-show-next
           "gk" 'elfeed-show-prev
           "C-j" 'elfeed-show-next
           "C-k" 'elfeed-show-prev
           ;; refresh
           "gr" 'elfeed-show-refresh
           ;; quit
           "q" 'elfeed-kill-buffer
           "ZQ" 'elfeed-kill-buffer
           "ZZ" 'elfeed-kill-buffer)
  (:states 'operator :keymaps 'elfeed-show-mode-map
           ;; Like `eww'.
           "u" '(menu-item
                 ""
                 nil
                 :filter (lambda (&optional _)
                           (when (memq evil-this-operator
                                       evil-collection-yank-operators)
                             (setq evil-inhibit-operator t)
                             #'elfeed-show-yank))))
  (dan/leader
    :keymaps 'override
    :states  '(normal motion)
    "r"      '(:ignore t :wk "[r]ss")
    "r e"    #'elfeed))


;; TODO https://github.com/HKey/feed-discovery/
