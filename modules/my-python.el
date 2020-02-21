;;; my-python.el -- Code for python configuration

;;; Commentary:
;; Nothiong special besides the user of djanog-test-runner
;; this package is not availbale on melpa so we need to configure it
;; locally.

;;; Code:
(add-to-list 'load-path "~/.emacs.d/vendor/django-test-runner/")
(load "django-test-runner.el")

(use-package python
  :init
  (require 'python)
  :bind (:map python-mode-map
          ("<f10>" . django-test-runner)
          ("<f2>" . py-isort-buffer))
  :config
  (add-hook 'python-mode-hook #'auto-virtualenv-set-virtualenv))

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))  ; or lsp-deferred

(use-package py-isort
  :ensure t)

(use-package pyvenv
  :ensure t)

(use-package auto-virtualenv
  :ensure t)

(provide 'my-python)
;;; my-python ends here
