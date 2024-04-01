;;; my-python.el -- Code for python configuration

;;; Commentary:
;; Nothiong special besides the user of djanog-test-runner
;; this package is not availbale on melpa so we need to configure it
;; locally.

;;; Code:
(use-package python
  :init
  (require 'python)
  :bind (:map python-mode-map
          ("<f2>" . py-isort-buffer))
  :config
  (add-hook 'python-mode-hook #'auto-virtualenv-set-virtualenv))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package blacken
  :bind (:map python-mode-map
              ("M-q" . blacken-buffer)))

(use-package py-isort
  :ensure t)

(use-package pyvenv
  :ensure t)

(use-package auto-virtualenv
  :ensure t)

(provide 'my-python)
;;; my-python ends here
