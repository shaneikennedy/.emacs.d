;;; my-docker -- Code for docker configuration

;;; Commentary:

;;; Code:
;;; Dockerfile Mode
(use-package dockerfile-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(provide 'my-docker)
;;; my-docker.el ends here
