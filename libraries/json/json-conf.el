;;; package --- summary:
;;; Commentary:
;;; -*- lexical-binding:t -*-

;;; Code:
(use-package json-mode
  :custom
  (json-mode-indent-level 2))

(use-package prettier-js
  :hook
  (json-mode . prettier-js-mode))


(provide 'json-conf)
;;; json-conf.el ends here
