;; Fix the node/npm version
(use-package nvm
  ;; FIXME Customize, help it find the nvm dir
  :after exec-path-from-shell
  :commands (nvm-use nvm-use-for)
  :init
  (defun dan/do-nvm-use (version)
    ;; TODO Change it so it reads .nvmrc
    (interactive "sVersion: ")
    (nvm-use version)
    (exec-path-from-shell-copy-env "PATH")))
