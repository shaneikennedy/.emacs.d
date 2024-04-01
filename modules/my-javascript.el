;;; my-javascript -- Code for javascript configuration

;;; Commentary:
;; Nothing special here, includes vue config

;;; Code:
(use-package vue-mode
  :config
  (setq mmm-submode-decoration-level 0)
  (add-hook 'vue-mode-hook #'lsp))

(use-package emmet-mode
  :hook (css-mode sgml-mode vue-mode js-jsx-mode js-mode))

(add-hook 'js-mode-hook 'lsp-deferred)
(add-hook 'js-jsx-mode-hook 'lsp-deferred)

(use-package typescript-mode)

(use-package prettier-js)

;; (quelpa '(npm :fetcher github :repo "shaneikennedy/npm.el"))
;; (require 'npm )

(provide 'my-javascript)
;;; my-javascript.el ends here
