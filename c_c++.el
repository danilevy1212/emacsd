;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

(add-hook 'c-mode-hook '(lambda ()
                          (require 'lsp)
                          (require 'lsp-clients)
                          (lsp)
                          (lsp-ui-doc-mode -1)
                          (lsp-ui-sideline-mode +1)))

;;; Code:
(provide 'c_c++)
;;; c_c++.el ends here
