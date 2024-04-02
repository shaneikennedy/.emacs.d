;;; my-rust.el -- Code for rust configuration

;;; Commentary:
;; Basic rust-mode setup

;;; Code:
(use-package rust-mode)

;; The Rust style guide recommends spaces rather than tabs for indentation
;; TAB should intend correctly for rust
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; I like format on save
(setq rust-format-on-save t)

;; Cargo commands run, build, test
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
(define-key rust-mode-map (kbd "C-c C-b") 'rust-build)
(define-key rust-mode-map (kbd "C-c C-t") 'rust-test)

;; LSP, duh.
(add-hook 'rust-mode-hook (lambda () (lsp)))

(provide 'my-rust)
;;; my-rust.el ends here
