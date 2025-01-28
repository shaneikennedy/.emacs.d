;;; my-java.el -- Code for java configuration

;;; Commentary:
;; Nothiong special besides the user of djanog-test-runner
;; this package is not availbale on melpa so we need to configure it
;; locally.

(use-package gradle-mode
  :config
  (setq gradle-gradlew-executable "./gradlew"))
(add-hook 'java-mode-hook 'gradle-mode)
(add-hook 'java-ts-mode-hook 'gradle-mode)

(provide 'my-java)
;;; my-java ends here
