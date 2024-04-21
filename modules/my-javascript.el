;;; my-javascript -- Code for javascript configuration

;;; Commentary:
;; Nothing special here, includes vue config

;;; Code:
(use-package typescript-mode)

(add-hook 'js-mode-hook (lambda () (lsp)))
(add-hook 'js-jsx-mode-hook (lambda () (lsp)))
(add-hook 'tsx-ts-mode (lambda () (lsp)))
(add-hook 'typescript-mode (lambda () (lsp)))

(provide 'my-javascript)
;;; my-javascript.el ends here
