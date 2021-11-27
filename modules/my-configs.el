;;; my-configs.el -- Code for config languages

;;; Commentary:

;;; Code:
(use-package yaml-mode)
(use-package jenkinsfile-mode)
(use-package toml-mode)
(use-package bazel)
(add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE\\'" . bazel-mode))
(use-package protobuf-mode)
(use-package terraform-mode)

(provide 'my-configs)

;;; my-configs.el ends here
