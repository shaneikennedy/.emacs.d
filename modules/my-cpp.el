;;; package --- Summary

;;; Commentary:

;;; Code:
(use-package lsp-mode :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))
(setq lsp-diagnostic-package nil)
(setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
(setq ccls-executable "/path/to/ccls/Release/ccls")

(provide 'my-cpp)
;;; my-cpp.el ends here
