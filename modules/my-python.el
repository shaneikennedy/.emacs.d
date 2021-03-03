;;; my-python.el -- Code for python configuration

;;; Commentary:
;; Nothiong special besides the user of djanog-test-runner
;; this package is not availbale on melpa so we need to configure it
;; locally.

;;; Code:
(quelpa '(django-test-runner :fetcher github :repo "bcfurtado/django-test-runner.el"))
(require 'django-test-runner )

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
