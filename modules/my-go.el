;;; my-go.el -- Code for go configuration

;;; Commentary:

;;; Code:
(use-package gotest
  :ensure t)

(use-package go-mode
  :ensure t
  :hook (go-mode . lsp-deferred)
  :bind (("C-c t" . go-test-current-test)))

(use-package go-ts-mode
  :hook
  (go-ts-mode . lsp-deferred))

(provide 'my-go)
;;; my-go.el ends here
