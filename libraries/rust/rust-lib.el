;; TODO This lib was setup in a bit of a hurry and I don't know if its the
;; most correct setup, check and make sure.

;; Syntax highlighting and other goodies.
(use-package rust-mode
  :mode
  (("\\.rs\\'" . rust-mode)))

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
