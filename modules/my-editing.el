;;; my-editing -- Code for editing configuration

;;; Commentary:

;;; Code:
;; Company is the best Emacs completion system.
(use-package company
  :bind (("C-." . company-complete))
  :diminish company-mode
  :custom
  (company-idle-delay 0)
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 10 "The more the merrier.")
  :config
  (global-company-mode)

  ;; use numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                                  `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))


(use-package company-prescient
  :config
  (company-prescient-mode t))


(use-package lsp-mode
  :commands lsp
  :ensure t
  :config
  (setq lsp-prefer-flymake nil
        lsp-headerline-breadcrumb-mode nil))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-diagnostic-max-lines 3
        lsp-ui-doc-enable t
        lsp-ui-doc-delay 3
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-diagnostics t ))

(use-package format-all)
(add-hook 'prog-mode-hook 'format-all-mode)
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode t))

(with-eval-after-load 'editorconfig
  (add-to-list 'editorconfig-indentation-alist
               '(vue-mode css-indent-offset
                          js-indent-level
                          sgml-basic-offset
                          ssass-tab-width)))

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
