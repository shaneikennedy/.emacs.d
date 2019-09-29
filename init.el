;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

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

(setq
 use-package-always-ensure t
 use-package-verbose t)

;; Fullscreen by default, as early as possible.

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; UTF-8 everywhere, please.

(prefer-coding-system 'utf-8)

(ignore-errors
  (set-frame-font "Iosevka-14"))

;; Any Customize-based settings should live in custom.el, not here.

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Always prefer newer files.

(setq load-prefer-newer t)

;; Disable otiose GUI settings: they just waste space.
;; fringe-mode is especially ruinous performance-wise.

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (fringe-mode -1))

;; Haven't figured out how to diminish eldoc-mode outside of
;; requiring this explicitly and doing it manually.

(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode))

;; Ensure GNU ELPA has the GPG keys it needs

(use-package gnu-elpa-keyring-update)

;; The Doom Emacs themes look really good.

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)

  ;; Docstrings should be a bit lighter, since they're important.
  (custom-theme-set-faces
  'doom-tomorrow-night
  '(font-lock-doc-face ((t (:foreground "#D8D2C1"))))))

;; Ensure that items in the PATH are made available to Emacs. This should
;; probably just come with the main distribution.

;; Recentf comes with Emacs but it should always be enabled.

(use-package recentf
  :init (recentf-mode t)
  :config
  (add-to-list 'recentf-exclude "\\.emacs.d")
  (add-to-list 'recentf-exclude ".+tmp......\\.org"))

;; Ivy makes most minibuffer prompts sortable and filterable. I used
;; to use helm, but it was too slow. Unfortunately org-ref depends on
;; it, but I never load it, so we good.

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (unbind-key "S-SPC" ivy-minibuffer-map)
  (setq ivy-height 15
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t)
  (defun swiper-at-point ()
    (interactive)
    (swiper (thing-at-point 'word)))
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         ("C-c s"   . swiper-at-point)
         ("C-s"     . swiper))
  :diminish)

;; ivy-rich makes Ivy look a little bit more like Helm.

(use-package ivy-rich
  :after counsel
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :init
  (ivy-rich-mode))

;; Provides visual interface to hydra layouts. I don't really
;; use hydras anywhere yet but some packages do.

(use-package ivy-hydra)

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; fish is a good shell. You should try it.

(use-package fish-mode)

;; Counsel applies Ivy-like behavior to other builtin features of
;; emacs, e.g. search.

(use-package counsel
  :ensure t
  :after ivy
  :init
  (counsel-mode 1)

  :bind (("C-c ;" . counsel-M-x)
         ("C-c U" . counsel-unicode-char)
         ("C-c i" . counsel-imenu)
         ("C-x f" . counsel-find-file)
         ("C-c y" . counsel-yank-pop)
	 ("C-c r" . counsel-recentf)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history))
  :diminish)

;; Deadgrep is amazing.

(use-package deadgrep
  :bind (("C-c h" . deadgrep)))

;; projectile comes with Emacs these days, but we want to enable
;; caching, since I work on big projects.

(use-package projectile
  :bind (("C-c f" . projectile-find-file))
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy)
  :diminish)

;; Counsel and projectile should work together.

(use-package counsel-projectile
  :bind (("C-c f" . counsel-projectile))
  :init
  (counsel-projectile-mode))

;; Sort commands by recency in ivy windows.

(use-package smex)

;; Keychain stuff. Note to self: if you keep having to enter your
;; keychain password on OS X, make sure that you have the following in .ssh/config:
;; Host *
;;    UseKeychain yes

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; Elm stuff.

(use-package elm-mode
  :disabled)

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
  (add-hook 'prog-mode-hook 'lsp)
  (setq lsp-prefer-flymake nil))

(use-package company-lsp
  :commands company-lsp)



;; Magit is one of the best pieces of OSS I have ever used. It is truly esssential.

(use-package magit
  :bind (("C-c g" . magit-status))
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :custom
  (magit-remote-set-if-missing t)
  (magit-diff-refine-hunk t)
  :config
  (magit-auto-revert-mode t)

  ;; Magit, and Emacs in general, has a nasty habit of prompting to save buffers
  ;; that are identical to those on disk. This is an attempt at remedying that,
  ;; one that I should probably attach to other functions like save-buffers-kill-emacs.
  (advice-add 'magit-refresh :before #'maybe-unset-buffer-modified)
  (advice-add 'magit-commit  :before #'maybe-unset-buffer-modified)
  (setq magit-completing-read-function 'ivy-completing-read)
  (add-to-list 'magit-no-confirm 'stage-all-changes))


;; Haskell and Elisp are made a lot easier when delimiters are nicely color-coded.

(use-package rainbow-delimiters
  :disabled
  :hook (prog-mode . rainbow-delimiters-mode))

;; multiple-cursors is better than cua-selection-mode.
;; TODO: learn ace-mc

(use-package multiple-cursors
  :bind (("C-c M" . mc/edit-lines)))

;; Common Haskell snippets. These take a while to load, so no need to block on startup.

(use-package haskell-snippets
  :defer yasnippet)

;; The beauty of undo-tree is that it means that, once you've typed something into a buffer,
;; you'll always be able to get it back. At least in theory. undo-tree has long-standing data
;; loss bugs that are unlikely to be fixed. But no other package provodes a comparable experience.

(use-package undo-tree
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map)
  :diminish)


;; I do all of my writing in either org-mode or markdown-mode.

(use-package markdown-mode
  :mode ("\\.md$" . gfm-mode)
  :config
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown -t html")))

;; Haskell is my programming language of choice.
(use-package haskell-mode
  :config

  (defun my-haskell-mode-hook ()
    "Make sure the compile command is right."
    (setq-local compile-command "stack build --fast"))

  (unbind-key "C-c C-s" haskell-mode-map)

  ;; I don't go overboard with the symbols but they can be nice.
  (setq haskell-font-lock-symbols 't
        haskell-font-lock-symbols-alist
        '(("\\" . "λ")
          ("<=" . "≤")
          (">=" . "≥")
          ("==" . "≡")
          ("<>" . "♢")
          ("/=" . "≢")
          ("*"  . "★")
          ("<=<" . "<=<")
;;          ("::" . "∷")
          ("<+>" . "⍚")
          ("undefined" . "⊥")
          ("forall" . "∀")
          ("." "∘" haskell-font-lock-dot-is-not-composition) ; or '◦'
          ))

  ;; Unfortunately haskell-mode doesn't quite track the latest and
  ;; greatest in Haskell extensions, so we have to give the font-lock
  ;; system a couple of hints.

  (append-to-list haskell-ghc-supported-extensions
                  '("DerivingVia" "BlockArguments" "DerivingStrategies"))

  (append-to-list haskell-font-lock-keywords '("capi" "via" "stock" "anyclass"))

  (append-to-list haskell-language-extensions
      '("-XDataKinds"
        "-XDeriveFoldable"
        "-XDeriveFunctor"
        "-XDeriveGeneric"
        "-XDeriveTraversable"
        "-XFlexibleContexts"
        "-XFlexibleInstances"
        "-XMonadFailDesugaring"
        "-XMultiParamTypeClasses"
        "-XOverloadedStrings"
        "-XRecordWildCards"
        "-XStandaloneDeriving"
        "-XStrictData"
        "-XTypeApplications"))

  :mode ("\\.hs$" . haskell-mode)
  :hook (haskell-mode . my-haskell-mode-hook)

  :bind (:map haskell-mode-map
         ("C-c a c" . haskell-cabal-visit-file)
	 ("C-c a b" . haskell-mode-stylish-buffer)
         ("C-c a i" . haskell-navigate-imports)
         ("C-c a a" . haskell-mode-toggle-scc-at-point)
         ("C-c a w" . stack-watch)))

(use-package hindent
  :ensure t)
(add-hook 'haskell-mode-hook #'hindent-mode)

(use-package flycheck-haskell
  :ensure t)
(add-hook 'haskell-mode-hook #'flycheck-haskell-setup)

(use-package company-ghc
  :ensure t)

(add-hook 'haskell-mode-hook 'company-mode)
(add-to-list 'company-backends 'company-ghc)

(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file user-init-file))

(defun kill-all-buffers ()
  "Close all buffers."
  (interactive)
  (maybe-unset-buffer-modified)
  (save-some-buffers)
  (mapc 'kill-buffer-with-prejudice (buffer-list)))

(defun split-right-and-enter ()
  "Split the window to the right and enter it."
  (interactive)
  (split-window-right)
  (other-window 1))

;; I'm not made of time, I can't type "yes" for every choice
(defalias 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode t)              ; Always highlight the current line.
(show-paren-mode t)                  ; And point out matching parentheses.
(delete-selection-mode t)            ; Behave like any other sensible text editor would.
(global-display-line-numbers-mode)   ; Emacs has this builtin now, it's fast
(save-place-mode)                    ; Remember where I was

;; Make sure that ligatures from fonts that offer them are enabled.
;; This isn't present on GNU Emacs and requires a tremendous amount
;; of munging of 'prettify-symbols-alist'.
(ignore-errors (mac-auto-operator-composition-mode))

(setq
  compilation-always-kill t              ; Never prompt to kill a compilation session.
  compilation-scroll-output 'first-error ; Always scroll to the bottom.
  make-backup-files nil                  ; No backups, thanks.
  auto-save-default nil                  ; Or autosaves. What's the difference between autosaves and backups?
  create-lockfiles nil                   ; Emacs sure loves to put lockfiles everywhere.
  inhibit-startup-screen t               ; No need to see GNU agitprop.
  kill-whole-line t                      ; Lets C-k delete the whole line
  mac-command-modifier 'super            ; I'm not sure this is the right toggle, but whatever.
  require-final-newline t                ; Auto-insert trailing newlines.
  ring-bell-function 'ignore             ; Do not ding. Ever.
  use-dialog-box nil                     ; Dialogues always go in the modeline.
  frame-title-format "emacs – %b"        ; Put something useful in the status bar.
  initial-scratch-message nil            ; SHUT UP SHUT UP SHUT UP
  mac-option-modifier 'meta              ; why isn't this the default
  save-interprogram-paste-before-kill t  ; preserve paste to system ring
  enable-recursive-minibuffers t         ; don't fucking freak out if I use the minibuffer twice
  sentence-end-double-space nil          ; are you fucking kidding me with this shit
  confirm-kill-processes nil             ; don't whine at me when I'm quitting.
  mac-mouse-wheel-smooth-scroll nil      ; no smooth scrolling
  mac-drawing-use-gcd t                  ; and you can do it on other frames
  mark-even-if-inactive nil              ; prevent really unintuitive undo behavior
  )

;; dired whines at you on macOS unless you do this.
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

(setq-default
  cursor-type 'bar
  indent-tabs-mode nil
  cursor-in-non-selected-windows nil)

(set-fill-column 95)

;; Always trim trailing whitespace.

(add-hook 'before-save-hook 'delete-trailing-whitespace)


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
                 (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
                 (setq evil-want-keybinding nil)
		 :config
		 (progn
		   (setq evil-leader/in-all-states t)
		   (evil-leader/set-key
		    "<SPC>" 'ace-window

		    ;; code commands
		    "c d" 'lsp-find-definition
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
		    "b" 'ido-switch-buffer
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

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init 'deadgrep))

(transparency 92)


(use-package drag-stuff
  :ensure t
  :bind* (("C-M-p" . drag-stuff-up)
          ("C-M-n" . drag-stuff-down)))

(use-package bind-key
  :ensure t
  :config
  (bind-key "M-j" (lambda () (interactive) (join-line -1))))

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


;; ZOOM
(use-package zoom
  :ensure t)

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

(add-to-list 'load-path "~/.emacs.d/vendor/django-test/")
(load "django-test.el")

(setq create-lockfiles nil)

(use-package py-isort
  :ensure t)

(use-package python
  :init
  (require 'python)
  :bind (:map python-mode-map
          ("C-<f9>" . mw/python--add-pudb-breakpoint)
          ("<f10>" . django-test-runner)
          ("C-M-<f9>" . mw/python--remove-breakpoints)
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
