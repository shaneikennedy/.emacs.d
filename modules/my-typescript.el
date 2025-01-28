;;; my-typescript -- Code for typescript configuration

;;; Commentary:
;; Nothing special here

;;; Code:
(use-package typescript-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'"    . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'"    . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'"    . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'"    . tsx-ts-mode))

(use-package npm)

(provide 'my-typescript)
;;; my-typescript.el ends here
