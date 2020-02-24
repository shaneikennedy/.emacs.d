;;; my-go.el -- Code for go configuration

;;; Commentary:

;;; Code:
(use-package go-mode
  :ensure t)

(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'before-save-hook #'gofmt-before-save)

(provide 'my-go)
;;; my-go.el ends here
