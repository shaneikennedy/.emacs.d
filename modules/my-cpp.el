;;; package --- Summary

;;; Commentary:

;;; Code:
(use-package lsp-mode :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

;; Run clang-format on save
(defun clang-format-save-hook-for-this-buffer ()
  "Create a buffer local save hook."
  (add-hook 'before-save-hook
    (lambda ()
      (progn
        (when (locate-dominating-file "." ".clang-format")
          (clang-format-buffer))
        ;; Continue to save.
        nil))
    nil
    ;; Buffer local hook.
    t))

;; Run this for each mode you want to use the hook.
(add-hook 'c-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))
(add-hook 'c++-mode-hook (lambda () (clang-format-save-hook-for-this-buffer)))

;; This has to be visible in PATH
(setq ccls-executable "ccls")

(defun lsp-file-watch-ignored ()
  lsp-file-watch-ignored)

;; Allow lsp-file-watch-ignored as a file or directory-local variable
(put 'lsp-file-watch-ignored 'safe-local-variable 'lsp--string-listp)

(provide 'my-cpp)
;;; my-cpp.el ends here
