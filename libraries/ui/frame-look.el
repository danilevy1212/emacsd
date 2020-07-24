;;; -*- lexical-binding:t -*-

;; The font of the hackermen.
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; NOTE Frame parameters are pretty big potential time savers!
;; Disable tool and scrollbars.
(unless (assq 'menu-bar-lines default-frame-alist)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars))
  (add-to-list 'default-frame-alist '(font . "Roboto Mono")))

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

;; x-gtk tooltips are ugly dude!
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

;; ;; Visually indicate matching pairs of parentheses."
;; (show-paren-mode t)
;; (setq show-paren-delay 0.05)
(use-package paren
  :straight
  (:type built-in)
  :custom
  (show-paren-delay 0.05)
  :config
  ;; TODO show paren mode cover expression for lisp modes
  (show-paren-mode t))

;; When you perform a problematic operation, flash the screen instead of ringing
;; the terminal bell.
(setq visible-bell t)

;; Set the default line length to 80.
(setq-default fill-column 80)

;; Show the line number on the side, but only in programming buffers.
(defun dan/set-relative-numbers-hook ()
  "Set the number line in a extra colummn."
  (setq display-line-numbers 'relative))

(add-hook 'prog-mode-hook #'dan/set-relative-numbers-hook)

;; focus moves to help window
(setq help-window-select t)

;; FIXME Put other customizations from `frame' here.
;; Frame customizations
(use-package frame
  :straight
  (:type built-in)
  :config
  ;; That blink is turning me crazy...
  (blink-cursor-mode -1))

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
  ;; FIXME https://www.gnu.org/software/emacs/manual/html_mono/efaq.html#Colors-on-a-TTY
  ;; Precludes system path searching:
  ;; $ export TERMINFO="$XDG_DATA_HOME"/terminfo
  ;; $ export TERMINFO_DIRS="$XDG_DATA_HOME"/terminfo:/usr/share/terminfo.
  :custom
  (nord-comment-brightness 20)
  (nord-region-highlight "snowstorm")
  (nord-uniform-mode-lines t)
  :custom-face
  (line-number-current-line ((t (:inherit line-number :foreground "white"))))
  :custom-face
  (show-paren-match ((t (:background unspecified :foreground unspecified :box (:color  "#88C0D0" :line-width 1)))))
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
  :straight
  (:type built-in)
  :config
  (column-number-mode))

(use-package battery
  :hook
  ;; FIXME Make it laptop specific
  ;; FIXME Further customize?
  '(after-init . display-battery-mode))

;; Describe what each key does while typing
(use-package which-key
  :defer 1
  :custom
  (which-key-idle-delay 0.1)
  (which-key-idle-secondary-delay 0.05)
  (which-key-show-early-on-C-h t)
  (which-key-allow-evil-operators t)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location '(bottom right))
  (which-key-show-prefix t)
  (which-key-show-remaining-keys t)
  :general
  (dan/local-leader
    :states  '(normal visual motion)
    :keymaps 'override
    "h k t"  #'which-key-show-top-level
    "h k k"  #'which-key-show-full-keymap
    "h k M"  #'which-key-show-full-major-mode
    "h k m"  #'which-key-show-minor-mode-keymap)
  :config
  (which-key-mode))

;; Highlight indentation
;; FIXME Customize!
;; (use-package highlight-indent-guides
;;   :defer t)

;; Highlight "TODO", "FIXME" words in comments.
(use-package hl-todo
  :commands hl-todo-mode
  :custom
  (hl-todo-keyword-faces  '(("HOLD" . "#d0bf8f")
                            ("TODO" . "#d0bf8f")
                            ("NEXT" . "#dca3a3")
                            ("THEM" . "#dc8cc3")
                            ("PROG" . "#7cb8bb")
                            ("OKAY" . "#7cb8bb")
                            ("DONT" . "#5f7f5f")
                            ("FAIL" . "#8c5353")
                            ("DONE" . "#afd8af")
                            ("NOTE" . "#d8dee9")
                            ("KLUDGE" . "#d0bf8f")
                            ("HACK" . "#d0bf8f")
                            ("TEMP" . "#d0bf8f")
                            ("FIXME" . "#cc9393")
                            ("XXX+" . "#cc9393")))
  :hook
  '(prog-mode . hl-todo-mode))

;; Highlight current line.
(use-package hl-line
  :straight
  (:type built-in)
  :config
  (global-hl-line-mode))

;; Never loose the cursor again!
(use-package beacon
  :defer 1
  :custom
  (beacon-blink-delay 0.1)
  (beacon-blink-duration 0.25)
  (beacon-blink-when-focused t)
  (beacon-size 50)
  :general
  (:states '(normal motion)
                      "C-M-s-SPC" #'beacon-blink))

;; Have you seen here dressed in blue?
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode-enable
  :hook
  '(prog-mode . rainbow-delimiters-mode-enable))

;; Use another frame to show linting errors
(use-package flycheck-posframe
  :if (display-graphic-p)
  :hook
  '(flycheck-mode . flycheck-posframe-mode))
