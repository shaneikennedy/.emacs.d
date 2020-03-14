;;; my-go.el -- Code for go configuration

;;; Commentary:

;;; Code:
(use-package gotest
  :ensure t)

(use-package go-mode
  :ensure t
  :bind (("C-c t" . go-test-current-test)))

(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'before-save-hook #'gofmt-before-save)

(provide 'my-go)
;;; my-go.el ends here
