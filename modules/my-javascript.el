;;; my-javascript -- Code for javascript configuration

;;; Commentary:
;; Nothing special here, includes vue config

;;; Code:
(use-package vue-mode
  :config
  (setq mmm-submode-decoration-level 0)
  (add-hook 'vue-mode-hook #'lsp))

(use-package emmet-mode
  :hook (css-mode sgml-mode vue-mode))

(use-package eslintd-fix
  :config
    (add-hook 'vue-mode-hook 'eslintd-fix-mode)
    (add-hook 'js-mode-hook 'eslintd-fix-mode))

(use-package typescript-mode)

(use-package prettier-js)

(quelpa '(npm :fetcher github :repo "shaneikennedy/npm.el"))
(require 'npm )

(defun sk/vue-base()
  "Snippet for base vue template."
  (interactive)
  (insert "<template>\n</template>
	    \n<script>\n export default {};\n</script>
	    \n\n<style scoped>\n</style>"))

(provide 'my-javascript)
;;; my-javascript.el ends here
