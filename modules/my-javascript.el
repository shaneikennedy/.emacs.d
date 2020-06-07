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

(use-package npm)
(use-package prettier-js)

(defun sk/vue-base()
  "Snippet for base vue template."
  (interactive)
  (insert "<template>\n</template>
	    \n<script>\n export default {};\n</script>
	    \n\n<style scoped>\n</style>"))

(provide 'my-javascript)
;;; my-javascript.el ends here
