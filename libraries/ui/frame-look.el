;; Sent font FIXME nicer fonts please
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-face-attribute 'default nil :font "Ubuntu Mono" :height 120)
(when (display-graphic-p)
  (set-fontset-font t 'latin "Ubuntu Mono"))

;; Disable tool and scrollbars.
(unless (assq 'menu-bar-lines default-frame-alist)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars))
  (menu-bar-mode -1))

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)

;; Precise window and frame resizing
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; No splash screen on init
(setq inhibit-splash-screen t)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1)

;; always avoid GUI
(setq use-dialog-box nil)

;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))
(setq x-gtk-use-system-tooltips nil)

;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 160
      split-height-threshold nil)

;; Allow for minibuffer-ception.
(setq enable-recursive-minibuffers t)

;; Indicate if we have hidden another minibuffer.
(minibuffer-depth-indicate-mode)

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.25)

;; Try really hard to keep the cursor from getting stuck in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether it has a noteable affect on Linux and Mac hasn't
;; been determined, but we inhibit it there anyway.
(setq inhibit-compacting-font-caches t)

;; Visually indicate matching pairs of parentheses."
(show-paren-mode t)
(setq show-paren-delay 0.05)

;; When you perform a problematic operation, flash the screen instead of ringing the terminal bell.
(setq visible-bell t)

;; Set the default line length to 80.
(setq-default fill-column 80)

;; Show the line number on the side, but only in programming buffers.
(defun dan/set-relative-numbers-hook ()
  "Hook to be runned after a initializating a programming mode, which will set the number line in a extra colummn."
  (setq display-line-numbers 'relative))

(add-hook 'prog-mode-hook #'dan/set-relative-numbers-hook)

;; focus moves to help window
(setq help-window-select t)

;; Dashboard splash screen
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  ;; FIXME create a `dan/dashboard-goto' interactive function.
  :custom
  (initial-buffer-choice (get-buffer "*dashboard*"))
  ;; FIXME Further customization here plz
  (dashboard-set-heading-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-file-icons t)
  (show-week-agenda-p t)
  (dashboard-items '((recents   . 5)
                     (bookmarks . 5)
                     (projects  . 5)
                     (agenda    . 5)))
  (dashboard-show-shortcuts t))

;; Unlock the power of the norse gods.
(use-package nord-theme
  :init
  (defun dan/set-background-when-in-terminal ()
    "Set the background color when in terminal Emacs, so it's consistent."
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "black" (selected-frame))))

  (add-hook 'window-setup-hook #'dan/set-background-when-in-terminal)

  :custom
  (nord-comment-brightness 20)
  (nord-region-highlight "snowstorm")
  (nord-uniform-mode-lines t)
  :config
  (load-theme 'nord t))

;; Modeline BUG Requires gitlab account through ssh
(use-package doom-modeline
  :custom
  (doom-modeline-modal-icon nil)
  :hook
  '(after-init . doom-modeline-mode))

;; FIXME Add all other options that come from simple.el here
;; Show column position in mode-line
(use-package simple
  :straight nil
  :config
  (column-number-mode))

(use-package battery
  :hook
  ;; FIXME Make it laptop specific
  ;; FIXME Further customize?
  '((after-init . display-battery-mode)))

;; Describe what each key does while typing
(use-package which-key
  :custom
  (which-key-idle-delay 0.1)
  (which-key-idle-secondary-delay 0.05)
  (which-key-show-early-on-C-h t)
  (which-key-allow-evil-operators t)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location '(bottom right))
  (which-key-show-prefix t)
  (which-key-show-remaining-keys t)
  :config
  (dan/local-leader
    :states  '(normal visual motion)
    :keymaps 'override
    "h k M"  #'which-key-show-full-major-mode
    "h k m"  #'which-key-show-minor-mode-keymap)
  (which-key-mode))

;; Highlight indentation
;; FIXME Customize!
;; https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :hook
  '((python-mode
     js-mode) . highlight-indent-guides-mode))

;; FIXME Customize!
;; Highlight TODO, FIXME words in comments FIXME Change to hl-mode
(use-package hl-todo
  :hook
  '(prog-mode . hl-todo-mode))

;; Never loose the cursor again!
(use-package beacon
  :custom
  (beacon-blink-delay 0.1)
  (beacon-blink-duration 0.25)
  (beacon-blink-when-focused t)
  (beacon-size 50)
  :config
  (general-define-key :states '(normal motion)
                      "C-M-s-SPC" #'beacon-blink)
  (beacon-mode))

(use-package rainbow-delimiters
  :hook
  '(prog-mode . rainbow-delimiters-mode-enable))

;; All the icons, to install -> (M-x all-the-icons-install-fonts)
(use-package all-the-icons)

;; All the icons for ivy helper.
(use-package all-the-icons-ivy-rich
  :if ivy-mode
  :init
  (all-the-icons-ivy-rich-mode +1)
  :custom
  (all-the-icons-ivy-rich-icon-size 1.0)
  ;; Definitions for ivy-rich transformers.
  ;; See `ivy-rich-display-transformers-list' for details."
  ;; FIXME Mega slow down when searching for a single dot, maybe auto-translate it to \.?
  ;; FIXME the culprit for the slowdown is `ivy--resize-minibuffer-to-fit', elp
  )

;; ivy-rich (Fancy ivy)
(use-package ivy-rich
  :if ivy-mode
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode +1))

;; Use another frame to show linting errors
(use-package flycheck-posframe
  :if (display-graphic-p)
  :hook
  '(flycheck-mode . flycheck-posframe-mode))
