;;; -*- lexical-binding:t -*-

;; Org basic defaults
(use-package org
  :straight org-plus-contrib (:no-byte-compile t)
  :general
  (dan/leader
    :states '(normal motion)
    :keymaps 'override
    "o" '(:ignore t :wk "[o]rg")
    "o a" #'org-agenda
    "o l" #'org-store-link)
  :config
  (put 'narrow-to-page 'disabled nil)
  (put 'set-goal-column 'disabled nil)
  (add-hook 'org-mode-hook 'auto-revert-mode)
  :custom
  (org-todo-keywords                                '((sequence "TODO(t)" "PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-todo-keyword-faces                           '(("PROGRESS" . "#E35DBF")
                                                    ("WAITING" . (:foreground "white" :background "#4d4d4d" :weight bold))
                                                    ("CANCELLED" . "#800000")))
  (org-directory                                    "~/Dropbox/org/")
  (org-agenda-files                                 `(,(expand-file-name "agenda.org" org-directory)))
  (org-agenda-dim-blocked-tasks                     nil)
  (org-agenda-todo-ignore-scheduled                 'future)
  (org-agenda-tags-todo-honor-ignore-options        t)
  (org-enforce-todo-dependencies                    t)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-skip-scheduled-if-deadline-is-shown   t)
  (org-agenda-span                                  'fortnight)
  (org-capture-templates                            `(("e" "Quick errands" entry (file+headline ,(car org-agenda-files) "Quick errands") "* TODO")))
  (org-agenda-window-setup                          'current-window)
  (org-agenda-skip-scheduled-if-done                nil)
  (org-default-priority                             ?C)
  (org-modules                                      '(
                                                      ;; org-w3m
                                                      org-habit
                                                      ;; org-bbdb
                                                      ;; org-bibtex
                                                      ;; org-docview
                                                      ;; org-gnus
                                                      ;; org-info
                                                      ;; org-irc
                                                      ;; org-mhe
                                                      ;; org-rmail
                                                      org-checklist))
  (org-agenda-custom-commands '(("n" "Agenda and all TODOs"
                                 ((alltodo "")
                                  (agenda  ""))))))

;;; Plugins
;; More control over how and when tasks change state
(use-package org-edna
  :hook
  '(org-mode . org-edna-mode))

;; Switch entry to DONE when all subentries are done, to TODO otherwise.
(add-hook 'org-after-todo-statistics-hook '(lambda (n-done n-not-done)
					     (let (org-log-done org-log-states)
					       (org-todo (if (= n-not-done 0) "DONE" "TODO")))))

;; Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise
(add-hook 'org-checkbox-statistics-hook  '(lambda ()
                                            (let ((todo-state (org-get-todo-state)) beg end)
                                              (unless (not todo-state)
                                                (save-excursion
                                                  (org-back-to-heading t)
                                                  (setq beg (point))
                                                  (end-of-line)
                                                  (setq end (point))
                                                  (goto-char beg)
                                                  (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                                                                         end t)
                                                      (if (match-end 1)
                                                          (if (equal (match-string 1) "100%")
                                                              (unless (string-equal todo-state "DONE")
                                                                (org-todo 'done))
                                                            (unless (string-equal todo-state "TODO")
                                                              (org-todo 'todo)))
                                                        (if (and (> (match-end 2) (match-beginning 2))
                                                                 (equal (match-string 2) (match-string 3)))
                                                            (unless (string-equal todo-state "DONE")
                                                              (org-todo 'done))
                                                          (unless (string-equal todo-state "TODO")
                                                            (org-todo 'todo))))))))))

;; Make org headings a bit more fashionable. FIXME Customization
;; (use-package org-superstar ; "prettier" bullets
;;   :config
;;   ;; Make leading stars truly invisible, by rendering them as spaces!
;;   (setq org-superstar-leading-bullet ?\s
;;         org-superstar-leading-fallback ?\s
;;         org-hide-leading-stars nil)
;;   ;; Don't do anything special for item bullets or TODOs by default; these slow
;;   ;; down larger org buffers.
;;   (setq org-superstar-prettify-item-bullets nil
;;         org-superstar-special-todo-items nil
;;         ;; ...but configure it in case the user wants it later
;;         org-superstar-todo-bullet-alist
;;         '(("TODO" . 9744)
;;           ("[ ]"  . 9744)
;;           ("DONE" . 9745)
;;           ("[X]"  . 9745)))
;;   (defun my-auto-lightweight-mode ()
;;     "Start Org Superstar differently depending on the number of lists items."
;;     (let ((list-items
;;            (count-matches "^[ \t]*?\\([+-]\\|[ \t]\\*\\)"
;;                           (point-min) (point-max))))
;;       (unless (< list-items 100)
;;         (org-superstar-toggle-lightweight-lists)))
;;     (org-superstar))

;;   (add-hook 'org-mode-hook #'my-auto-lightweight-mode))

;; https://github.com/Titan-C/org-cv
;; (use-package ox-altacv
;;   :straight
;;   (:host gitlab :repo "Titan-C/org-cv" :branch "master"))

;; ALTERNATIVE --> https://github.com/liantze/AltaCV, https://titan-c.gitlab.io/org-cv/

;; Manage elfeed from an org file!
(use-package elfeed-org
  :custom
  (rmh-elfeed-org-files (list (concat org-directory "elfeed.org")))
  :config
  (elfeed-org)
  :general
  (dan/leader
    :keymaps 'override
    :states  '(normal motion)
    "r p"    `((lambda ()
                 (interactive)
                 (find-file ,(car rmh-elfeed-org-files))) :wk "[p]rivate config")))

;; Vim keys on org-mode
(use-package evil-org
  :config
  (defun my-org-agenda-hook ()
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook 'evil-org-set-key-theme)
  (add-hook 'org-agenda-mode-hook 'my-org-agenda-hook))

;; Preview current org file in eww whenever you save it.
(use-package org-preview-html)
