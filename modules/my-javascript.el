;;; my-javascript -- Code for javascript configuration

;;; Commentary:
;; Nothing special here, includes vue config

;;; Code:
(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (setq mmm-submode-decoration-level 0))

(use-package js2-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(provide 'my-javascript)
;;; my-javascript.el ends here
