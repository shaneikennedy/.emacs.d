;;; my-python.el -- Code for python configuration

;;; Commentary:
;; Nothiong special besides the user of djanog-test-runner
;; this package is not availbale on melpa so we need to configure it
;; locally.

;;; Code:
(use-package python)
(use-package py-isort
  :ensure t)

(use-package pyvenv
  :ensure t)

(use-package auto-virtualenv
  :ensure t)

(provide 'my-python)
;;; my-python ends here
