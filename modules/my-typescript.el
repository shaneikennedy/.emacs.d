;;; my-typescript -- Code for typescript configuration

;;; Commentary:
;; Nothing special here

;;; Code:
(use-package typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'typescript-ts-mode-hook #'lsp)

(use-package npm)

(provide 'my-typescript)
;;; my-typescript.el ends here
