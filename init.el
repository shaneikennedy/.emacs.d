;; Package-initialization preamble, adding melpa and melpa-stable.

(require 'package)

(defmacro append-to-list (target suffix)
  "Append SUFFIX to TARGET in place."
  `(setq ,target (append ,target ,suffix)))

(append-to-list package-archives
                '(("melpa" . "http://melpa.org/packages/")
                  ("melpa-stable" . "http://stable.melpa.org/packages/")
                  ("org-elpa" . "https://orgmode.org/elpa/")))

(package-initialize)

;; Ensure use-package is present. From here on out, all packages are loaded
;; with use-package.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Allow navigation between use-package stanzas with imenu.
;; This has to be set before loading use-package.
(defvar use-package-enable-imenu-support t)

(require 'use-package)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'my-gui)
(require 'my-python)
(require 'my-javascript)
(require 'my-haskell)


(setq
 use-package-always-ensure t
 use-package-verbose t)

;; Any Customize-based settings should live in custom.el, not here.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Always prefer newer files.
(setq load-prefer-newer t)

;; Company is the best Emacs completion system.

(use-package company
  :bind (("C-." . company-complete))
  :diminish company-mode
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-abort-manual-when-too-short t "Be less enthusiastic about completion.")
  :config
  (global-company-mode)

  ;; use numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))

(use-package lsp-mode
  :commands lsp
  :ensure t
  :config
  (require 'lsp-clients)
  (setq lsp-prefer-flymake nil
        lsp-enable-snippet nil))


(use-package company-lsp
  :commands company-lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)


;; I do all of my writing in either org-mode or markdown-mode.

(use-package markdown-mode
  :mode ("\\.md$" . gfm-mode)
  :config
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown -t html")))

(defun sk/vue-base()
  "Snippet for base vue template."
  (interactive)
  (insert "<template>\n</template>
	    \n<script>\n export default {};\n</script>
	    \n\n<style scoped>\n</style>"))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

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
                 (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
		 :config
		 (progn
		   (setq evil-leader/in-all-states t)
		   (evil-leader/set-key
		    "<SPC>" 'ace-window

		    ;; code commands
		    "c d" 'lsp-find-definition
                    "c i" 'counsel-imenu
		    ";" 'comment-line

		    ;; projectile commands
		    "p r" 'deadgrep
		    "p f" 'projectile-find-file
		    "p p" 'projectile-switch-project

		    ;; magit shortcuts
		    "g s" 'magit-status
		    "g c" 'magit-checkout
		    "g b" 'magit-blame

		    ;; file operations
		    "f w" 'save-buffer
		    "f f" 'ido-find-file
		    "f s" 'swiper
		    "f c" 'copy-file-name-to-clipboard

		    ;; buffer operations
		    "b" 'ivy-switch-buffer
		    "k" 'kill-buffer
		    "K" 'kill-this-buffer

		    ;; shell
		    "T" 'shell

		    ;; window commands
		    "w d" 'delete-window
		    "w n" 'split-right-and-enter
		    "w 1" 'delete-other-windows

		    ))
		    (use-package evil-magit
		    :ensure t)
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

;;; Dockerfile Mode
(use-package dockerfile-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;;; Editor
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode t))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'vue-mode))


(provide 'init)
;;; init.el ends here
