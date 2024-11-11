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

(add-hook 'go-ts-mode-hook 'go-mode)
(add-hook 'go-mode-hook (lambda ()
                          (flycheck-add-next-checker 'lsp 'go-vet)
                          (flycheck-add-next-checker 'lsp 'go-staticcheck)))
(provide 'my-go)
;;; my-go.el ends here
