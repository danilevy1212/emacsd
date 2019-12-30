;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;;; ORG-MODE config

;;; Code:
;; Org basic defaults
(use-package org
  ;; :after doom-modeline
  :hook
  (org-mode . auto-revert-mode)
  :config
  (put 'narrow-to-page 'disabled nil)
  (put 'set-goal-column 'disabled nil)
  (require 'org-checklist "~/.emacs.d/.config/org-checklist")
  :custom
  (org-todo-keywords      '((sequencep "TODO(t)" "PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-todo-keyword-faces '(("PROGRESS" . "#E35DBF")
                            ("WAITING" . (:foreground "white" :background "#4d4d4d" :weight bold))
                            ("CANCELLED" . "#800000")))
  (org-agenda-files        '("~/Dropbox/org/agenda.org"))
  (org-agenda-dim-blocked-tasks                     nil)
  (org-agenda-todo-ignore-scheduled                 'future)
  (org-agenda-tags-todo-honor-ignore-options        t)
  (org-enforce-todo-dependencies                    t)
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-skip-scheduled-if-deadline-is-shown   t)
  (org-agenda-span                                  'fortnight)
  (org-capture-templates                            '(("e" "Quick errands" entry (file+headline "~/Dropbox/org/agenda.org" "Quick errands") "* TODO")))
  (org-agenda-window-setup                          'only-window)
  (org-agenda-skip-scheduled-if-done                nil)
  (org-default-priority                             ?C)
(org-modules                                      '(org-w3m
						    org-habit
						    org-bbdb
						    org-bibtex
						    org-docview
						    org-gnus
						    org-info
						    org-irc
						    org-mhe
						    org-rmail
						    org-habit
						    org-checklist)))

;;; Plugins
;; More control over how and when tasks change state
(use-package org-edna
  :config
  (org-edna-load))


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


;; Init screen is agenda-view
(add-hook 'after-init-hook (lambda() (org-agenda nil "n")) t)

(provide 'orgconf)
;;; orgconf.el ends here
