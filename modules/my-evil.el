;;; my-evil -- Code for evil configuration

;;; Commentary:

;;; Code:
;; Evil configuration
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (progn
    (setq evil-default-cursor t)
    (use-package evil-leader
      :ensure t
      :init (global-evil-leader-mode)
      (evil-leader/set-leader "<SPC>")
      (setq evil-undo-system 'undo-fu)
      (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
      :config
      (progn
	(setq evil-leader/in-all-states t)
	(evil-leader/set-key
	  "<SPC>" 'ace-window

	  ;; code commands
	  "c a" 'eglot-code-actions
	  "c d" 'xref-find-definitions
          "c r" 'eglot-rename
          "c u" 'xref-find-references
          "c s" 'imenu
          "c f" 'eglot-format-buffer
          "c h" 'eldoc-box-help-at-point
	  ";" 'comment-line

	  ;; project commands
	  "p R" 'deadgrep
	  "p f" 'projectile-find-file
	  "p p" 'projectile-switch-project
          "p r" 'counsel-projectile-rg

	  ;; magit shortcuts
	  "g s" 'magit-status
	  "g h" 'magithub-dashboard
	  "g c" 'magit-checkout
	  "g b" 'magit-blame
          "g l" 'magit-log-buffer-file

	  ;; file operations
	  "f w" 'save-buffer
	  "f f" 'counsel-find-file
	  "f s" 'swiper
	  "f S" 'swiper-at-point
	  "f c" 'copy-file-name-to-clipboard

	  ;; buffer operations
	  "b" 'ivy-switch-buffer
	  "k" 'kill-buffer
	  "K" 'kill-this-buffer

	  ;; shell
	  "T" 'shell

          ;; macros
          "m s" 'start-kbd-macro
          "m e" 'end-kbd-macro
          "m p" 'sk/apply-macro-page

          ;; errors
          "e n" 'flymake-goto-next-error
          "e p" 'flymake-goto-prev-error

	  ;; window commands
	  "w d" 'delete-window
	  "w n" 'split-right-and-enter
	  "w 1" 'delete-other-windows

	  ))
      (modify-syntax-entry ?_ "w")

      (use-package evil-surround
	:ensure t
	:config
	(global-evil-surround-mode 1))

      (use-package evil-org
	:ensure t)
      ;; boot evil by default

      (use-package evil-collection
        :after evil
        :ensure t
        :config
        (evil-collection-init))

      (evil-mode 1)))
  :config
  (progn
    ;; escape key should always escacpe
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

    ;; modes to map to diifferent default states
    (dolist (mode-map '((comint-mode . emacs)
                        (dashboard-mode . emacs)
			(term-mode . emacs)
			(help-mode . emacs)
			(fundamental-mode . emacs)))
      (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))
    ))

(provide 'my-evil)
;;; my-evil.el ends here
