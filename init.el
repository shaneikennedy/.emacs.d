;; My emacs configuration
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; ensure that use-package is installed
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;; Custom functions
(defun sk/eslint-run-autofix()
  "Run eslint autofix on file."
  (interactive)
  (start-process-shell-command "eslint fix"
			       "npx"
			       (concat
				(projectile-project-root)
				"node_modules/eslint/bin/eslint.js --fix " (buffer-file-name))))


(defun sk/django-test-string()
  "Insert django test string in buffer"
  (interactive)
  (insert "export DJANGO_SETTINGS_MODULE=\"kog.config.settings.testing\" && python manage.py test"))

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

(defun transparency (value)
   "Set the transparency of the frame window given a VALUE, 0=transparent/100=opaque."
   (interactive "nTransparency value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))

;; Evil configuration
(use-package evil
  :ensure t
  :init
  (progn
    (setq evil-default-cursor t)
    (use-package evil-leader
		 :ensure t
		 :init (global-evil-leader-mode)
		 (evil-leader/set-leader "<SPC>")
		 :config
		 (progn
		   (setq evil-leader/in-all-states t)
		   (evil-leader/set-key
		    "<SPC>" 'ace-window

		    ;; code commands
		    "c d" 'lsp-find-definition
		    ;; projectile commands
		    "p r" 'projectile-ripgrep
		    "p f" 'projectile-find-file
		    "p p" 'projectile-switch-project

		    ;; magit shortcuts
		    "g s" 'magit-status
		    "g c" 'magit-checkout

		    ;; file operations
		    "f w" 'save-buffer
		    "f f" 'ido-find-file
		    "f s" 'swiper
		    ";" 'comment-line
		    "b" 'ido-switch-buffer
		    "e" 'er/expand-region
		    "k" 'kill-buffer
		    "K" 'kill-this-buffer
		    "T" 'shell

		    ;; window commands
		    "w d" 'delete-window
		    "w n" 'split-window-horizontally

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
			(term-mode . emacs)
			(help-mode . emacs)
			(fundamental-mode . emacs)))
      (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))
    ))
(evil-mode 1)

;;; UI config
;;;;

;; Disable toolbar & menubar & scroll-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(defun set-frame-maximized (&optional frame)
  "Initialize FRAME to take full window size."
  (set-frame-parameter frame 'fullscreen 'maximized))

(add-hook 'window-setup-hook 'set-frame-maximized)
(add-hook 'after-make-frame-functions 'set-frame-maximized)
(transparency 92)

;; (use-package monokai-theme
;;   :ensure t
;;   :init
;;   (load-theme 'monokai t))
(add-to-list 'load-path "~/.emacs.d/themes/")
(load "darkplus-theme")
;; (load "dracula-theme")

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))
(setq doom-modeline-buffer-file-name-style 'relative-to-project)

(set-frame-font "Menlo-12" t t)
(setq inhibit-startup-screen 1)
(setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("08141ce5483bc173c3503d9e3517fca2fb3229293c87dc05d49c4f3f5625e1df" default)))
 '(package-selected-packages
   (quote
    (ms-python lsp-python-ms lsp-common lsp-python doom-modeline yaml-mode lsp-ui company-lsp company flycheck editorconfig js2-mode vue-mode auto-virtualenv pyvenv py-isort diff-hl projectile-ripgrep projectile exec-path-from-shell dockerfile-mode counsel ace-window super-save ivy drag-stuff smartparens monokai-theme use-package evil-surround evil-org evil-magit evil-leader))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:background "#1e1e1e" :foreground "#d4d4d4")))))

;;; Editor config
;;;
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :bind ("M-s" . sp-splice-sexp)
  :init
  (smartparens-global-mode t)
  (smartparens-strict-mode t)
  (show-smartparens-global-mode t)
  (require 'smartparens-config))

(use-package drag-stuff
  :ensure t
  :bind* (("C-M-p" . drag-stuff-up)
          ("C-M-n" . drag-stuff-down)))

(use-package bind-key
  :ensure t
  :config
  (bind-key "M-j" (lambda () (interactive) (join-line -1))))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode t)
  (setq ivy-use-selectable-prompt t))

(use-package counsel
  :ensure t
  :bind (("C-s" . swiper)
         ("C-x C-f" . counsel-find-file)
         ("M-x" . counsel-M-x)
         ("M-i" . counsel-imenu)
         ("C-c s a" . counsel-ag)
         ("C-c s g" . counsel-git-grep)
         ("C-c s r" . counsel-rg)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package super-save
  :ensure t
  :diminish super-save-mode
  :config
  (setq super-save-auto-save-when-idle t
    super-save-idle-duration 5)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-triggers 'magit-status)
  (super-save-mode +1))

(use-package ace-window
  :ensure t
  :diminish ace-window-mode
  :bind
  ("M-o" . ace-window)
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "deep sky blue"
                      :weight 'bold
                      :height 2.0)
  (set-face-attribute 'aw-mode-line-face nil
                      :inherit 'mode-line-buffer-id
                      :foreground "lawn green")
  (setq aw-scope 'frame)
  (setq aw-dispatch-always t)
  (setq aw-keys '(?q ?w ?e ?r ?a ?s ?d ?f))
  (setq aw-dispatch-alist '((?c aw-swap-window "Ace - Swap Window")
                            (?n aw-flip-window)))
  (ace-window-display-mode t))

(use-package ibuffer
  :ensure t)

;; prefer vertical splits
(setq split-width-threshold 1)

;; Don't save temporary files everywhere
(setq backup-directory-alist `(("." . "~/.emacs.d/.saves")))

;; Don't break lines for me, please
(setq-default truncate-lines t)


;;; Dockerfile Mode
(use-package dockerfile-mode
  :ensure t)

;;; OS specific config
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq default-input-method "MacOSX"))

(when *is-a-linux*
  (setq x-super-keysym 'meta))		; Set Super key as Meta

;; Enable a better look and feel when on mac os by adding natural
;; title bar
(when *is-a-mac*
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(use-package exec-path-from-shell
  :ensure t
  :if *is-a-mac*
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))


;;; Projectile

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy))

(use-package projectile-ripgrep
  :ensure t)

;;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
          ("C-c v b" . magit-blame))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

(use-package diff-hl
  :ensure t
  :demand t
  :config
    (diff-hl-flydiff-mode +1)
    (global-diff-hl-mode +1)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote))

;;; Python

(setq create-lockfiles nil)

(use-package py-isort
  :ensure t)

(use-package python
  :init
  (require 'python)
  :bind (:map python-mode-map
          ("C-<f9>" . mw/python--add-pudb-breakpoint)
          ("C-M-<f9>" . mw/python--remove-breakpoints)
          ("<f7>" . bc/test-django-function)
          ("C-M-f" . sp-forward-sexp)
          ("C-M-b" . sp-backward-sexp)
          ("C-c C-t o" . py-isort-buffer))
  :config
  (add-hook 'python-mode-hook #'auto-virtualenv-set-virtualenv))

(use-package pyvenv
  :ensure t)

(use-package auto-virtualenv
  :ensure t)

(use-package dired-x
  :config
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files
    (concat dired-omit-files "$\\|^__pycache__$\\|^\\.pyc$\\|^\\.DS_Store$"))
  )


;;; Javascript
(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (setq mmm-submode-decoration-level 0))
;; (add-hook 'vue-mode-hook
;; 	  (lambda()
;; 	    (add-hook 'before-save-hook 'sk/eslint-run-autofix nil 'make-it-local)))


(use-package js2-mode
  :ensure t)
(add-hook 'js2-mode-hook 'lsp)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-hook 'js2-mode-hook
;; 	  (lambda()
;; 	    (add-hook 'before-save-hook 'sk/eslint-run-autofix nil 'make-it-local)))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)


;;; YAML mode
(use-package yaml-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

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

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  "Check for local eslintrc."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(use-package company
  :ensure t
  :config
  (company-mode))

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

(use-package lsp-mode
  :commands lsp
  :ensure t
  :config
  (require 'lsp-clients)
  (add-hook 'prog-mode-hook 'lsp)
  (setq lsp-prefer-flymake nil))

(use-package company-lsp
  :commands company-lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil))


(provide 'init)
;;; init.el ends here
