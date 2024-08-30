;;; my-java.el -- Code for java configuration

;;; Commentary:
;; Nothiong special besides the user of djanog-test-runner
;; this package is not availbale on melpa so we need to configure it
;; locally.

;;; Code:
(use-package lsp-java)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-ts-mode-hook #'lsp)
(require 'lsp-java-boot) ;; springboot
;; to enable the lenses
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
(add-hook 'java-ts-mode-hook #'lsp-java-boot-lens-mode)

;; needed for java 21
(setq lsp-java-jdt-download-url
      "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.37.0/jdt-language-server-1.37.0-202406271335.tar.gz")

(use-package gradle-mode
  :config
  (setq gradle-gradlew-executable "./gradlew"))
(add-hook 'java-mode-hook 'gradle-mode)
(add-hook 'java-ts-mode-hook 'gradle-mode)

(provide 'my-java)
;;; my-java ends here
