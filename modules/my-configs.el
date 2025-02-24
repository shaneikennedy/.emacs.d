;;; my-configs.el -- Code for config languages

;;; Commentary:

;;; Code:
(use-package yaml-mode)
(use-package jenkinsfile-mode)
(use-package toml-mode)
(use-package protobuf-mode)
(use-package terraform-mode)
(add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(provide 'my-configs)

;;; my-configs.el ends here
