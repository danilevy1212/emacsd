;; TODO This lib was setup in a bit of a hurry and I don't know if its the
;; TODO https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/rust/config.el INSPIRATION!
;; most correct setup, check and make sure.

;; Syntax highlighting and other goodies.
;; (use-package rust-mode
;;   :if (not (featurep 'lsp-mode))
;;   :mode
;;   (("\\.rs\\'" . rust-mode)))


;; TODO Customize!
;; Syntax highlighting and other goodies.
(use-package rustic
  :config
  (defun dan/rustic-use-stable-channel-clippy ()
    (when (derived-mode-p 'rustic-mode)
      ;; FIXME rustic mode customization may make this unnecesary
      (progn
        (when (not (memq 'rustic-clippy flycheck-checkers))
          (push 'rustic-clippy flycheck-checkers))
        (setq rustic-flycheck-clippy-params "--message-format=json"))))
  (add-hook 'flycheck-mode-hook #'dan/rustic-use-stable-channel-clippy)
  (evil-set-initial-state #'rustic-popup-mode #'insert))

;; Make sense of those configs!
(use-package toml-mode
  :mode
  (("\\.toml\\'" . toml-mode)))

;; FIXME flycheck-rust-setup is not working, I think it's because it cannot find
;; the Cargo.toml file, but until I don't learn to `edebug' I can't tell for sure.
;; Syntax error checking
;; (use-package flycheck-rust
;;   :hook
;;   '(flycheck-mode . flycheck-rust-setup))
