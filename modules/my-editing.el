;;; my-editing -- Code for editing configuration

;;; Commentary:

;;; Code:
;; Company is the best Emacs completion system.
(use-package company
  :bind (("C-." . company-complete))
  :diminish company-mode
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-abort-manual-when-too-short t "Be less enthusiastic about completion.")
  :config
  (global-company-mode)

  ;; use numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))

(use-package lsp-mode
  :commands lsp
  :ensure t
  :config
  (require 'lsp-clients)
  (setq lsp-prefer-flymake nil
        lsp-enable-snippet nil))


(use-package company-lsp
  :commands company-lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode t))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'vue-mode))

(provide 'my-editing)
;;; my-editing.el ends here
