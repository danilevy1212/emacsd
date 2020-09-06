;; Typescript hightlighting
(use-package typescript-mode
  :commands typescript-mode
  :config
  (when (fboundp #'lsp-deferred)
    (add-hook 'typescript-mode-hook #'lsp-deferred)))
