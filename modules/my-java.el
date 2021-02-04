;;; my-java -- Code for java configuration

;;; Commentary:
;; Nothing special here

;;; Code:
(use-package lsp-java
    :config
    (setq lsp-java-jdt-download-url  "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz")
    (add-hook 'java-mode-hook 'lsp))

(use-package dap-java
    :ensure nil
    :bind (:map java-mode-map
          ("<f10>" . dap-java-run-test-class)))

(provide 'my-java)
;;; my-java.el ends here
