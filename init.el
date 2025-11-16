(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Configure package archives
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/packages/")))

(setq use-package-always-ensure t)
;; Quality of life defaults
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

(electric-pair-mode)
(setq
 mac-option-key-is-meta nil
 mac-command-key-is-meta t
 mac-command-modifier 'meta
 mac-option-modifier 'none
 default-input-method "MacOSX"
 gc-cons-threshold 100000000            ; Bump garbage collection threshold to 100mb
 compilation-always-kill t              ; Never prompt to kill a compilation session.
 compilation-scroll-output 'first-error ; Always scroll to the bottom.
 make-backup-files nil                  ; No backups, thanks.
 auto-save-default nil                  ; Or autosaves. What's the difference between autosaves and backups?
 create-lockfiles nil                   ; Emacs sure loves to put lockfiles everywhere.
 inhibit-startup-screen t               ; No need to see GNU agitprop.
 kill-whole-line t                      ; Lets C-k delete the whole line
 require-final-newline t                ; Auto-insert trailing newlines.
 ring-bell-function 'ignore             ; Do not ding. Ever.
 use-dialog-box nil                     ; Dialogues always go in the modeline.
 frame-title-format "emacs – %b"        ; Put something useful in the status bar.
 initial-scratch-message nil            ; SHUT UP SHUT UP SHUT UP
 save-interprogram-paste-before-kill t  ; preserve paste to system ring
 enable-recursive-minibuffers t         ; don't fucking freak out if I use the minibuffer twice
 sentence-end-double-space nil          ; are you fucking kidding me with this shit
 confirm-kill-processes nil             ; don't whine at me when I'm quitting.
 mac-mouse-wheel-smooth-scroll nil      ; no smooth scrolling
 mac-drawing-use-gcd t                  ; and you can do it on other frames
 mark-even-if-inactive nil              ; prevent really unintuitive undo behavior
 )
;; Any Customize-based settings should live in custom.el, not here.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))
;; Actual emacs packages to setup
(use-package diminish)


(use-package git-modes)
(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :custom
  (magit-remote-set-if-missing t)
  (magit-diff-refine-hunk t)
  :config
  (magit-auto-revert-mode t)
  (add-to-list 'magit-no-confirm 'stage-all-changes))



(which-key-mode)
(which-key-setup-minibuffer)
;; Optional for vim bindings
(defun split-right-and-enter ()
  "Split the window to the right and enter it."
  (interactive)
  (split-window-right)
  (other-window 1))

(ignore-errors
  (set-frame-font "Geist Mono-13"))

(use-package zoom
  :diminish
  :config
  (zoom-mode t))

(use-package ace-window
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


(use-package eldoc-box
  :diminish
  :ensure t)

;; Enable Vertico.
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package consult
  :ensure t
   :hook (completion-list-mode . consult-preview-at-point-mode)
   :custom
   (consult-preview-key nil)
   (consult-narrow-key nil)
   :config
   (consult-customize consult-theme consult-line consult-line-at-point :preview-key '(:debounce 0.2 any))
 )

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ("C-n" . corfu-next)
        ([tab] . corfu-next)
        ("C-p" . corfu-previous)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :custom
  (tab-always-indent 'complete)
  (corfu-auto t)
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  (corfu-popupinfo-delay '(0.2 . 0.1))
  (corfu-popupinfo-direction '(right left vertical))
  (corfu-popupinfo-max-width 80)
  (corfu-popupinfo-max-height 20)
  (corfu-popupinfo-resize t)

  ;; Enable Corfu
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package cape
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package rust-mode)

(use-package rustic
  :config
  (setq rustic-lsp-client 'eglot)
  :after (rust-mode))

(use-package gruvbox-theme)
(load-theme 'modus-vivendi)
(set-cursor-color "yellow")

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

;; Deadgrep is amazing.
(use-package deadgrep
  :bind (("C-c h" . deadgrep)))

(use-package kkp
  :config
  (global-kkp-mode 1))

(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file user-init-file))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-items '((recents  . 5)
                          (projects . 10))
        dashboard-set-footer nil))

(use-package all-the-icons)
(use-package all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook (lambda() 'dired-hide-details-mode 1))

(use-package drag-stuff
  :ensure t
  :bind* (("C-M-p" . drag-stuff-up)
          ("C-M-n" . drag-stuff-down)))


(use-package diff-hl
  :ensure t
  :demand t
  :config
  (diff-hl-flydiff-mode +1)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote))


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package groovy-mode)





(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
 (evil-set-leader 'normal (kbd "SPC"))
 (evil-define-key 'normal 'global (kbd "<leader>fw") 'save-buffer)
 (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
 (evil-define-key 'normal 'global (kbd "<leader>wn") 'split-right-and-enter)
 (evil-define-key 'normal 'global (kbd "<leader><SPC>") 'ace-window)
 (evil-define-key 'normal 'global (kbd "<leader>wd") 'delete-window)
 (evil-define-key 'normal 'global (kbd "<leader>w1") 'delete-other-windows)
 (evil-define-key 'normal 'global (kbd "<leader>b") 'consult-buffer)
 (evil-define-key 'normal 'global (kbd "<leader>fs") 'consult-line)
 (evil-define-key 'normal 'global (kbd "<leader>fS") 'consult-line-at-point)
 (evil-define-key 'normal 'global (kbd "<leader>pp") 'project-switch-project)
 (evil-define-key 'normal 'global (kbd "<leader>pr") 'consult-ripgrep)
 (evil-define-key 'normal 'global (kbd "<leader>pR") 'deadgrep)
 (evil-define-key 'normal 'global (kbd "<leader>pf") 'consult-fd)
 (evil-define-key 'normal 'global (kbd "<leader>;") 'comment-line)
 (evil-define-key 'normal 'global (kbd "<leader>ca") 'eglot-code-actions)
 (evil-define-key 'normal 'global (kbd "<leader>cd") 'xref-find-definitions)
 (evil-define-key 'normal 'global (kbd "<leader>cu") 'xref-find-references)
 (evil-define-key 'normal 'global (kbd "<leader>cr") 'eglot-rename)
 (evil-define-key 'normal 'global (kbd "<leader>ch") 'eldoc-box-help-at-point)
 (evil-define-key 'normal 'global (kbd "<leader>lc") 'ellama-chat)
 (evil-define-key 'normal 'global (kbd "<leader>ll") 'ellama)
 (evil-define-key 'normal 'global (kbd "<leader>gs") 'magit-status)
 (evil-define-key 'normal 'global (kbd "<leader>gc") 'magit-checkout)
 (evil-define-key 'normal 'global (kbd "<leader>gb") 'magit-blame)
 (evil-define-key 'normal 'global (kbd "<leader>gl") 'magit-log-buffer-file)
 (evil-define-key 'normal 'global (kbd "<leader>en") 'flymake-goto-next-error)
 (evil-define-key 'normal 'global (kbd "<leader>ep") 'flymake-goto-prev-error)
 (evil-define-key 'normal 'global (kbd "<leader>s") 'yas-insert-snippet)
 (evil-define-key 'normal 'global (kbd "<leader>T") 'shell)
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
    )
  (evil-mode)
  )

(use-package doom-themes)

(defun consult-line-at-point ()
  (interactive)
    (consult-line (word-at-point)))

(straight-use-package
  '(amp :type git :host github :repo "shaneikennedy/amp.el"))

(use-package evil-collection
  :after evil
  :diminish
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :after evil)

(add-hook 'magit-status-mode-hook
          (lambda()
            (local-unset-key (kbd "SPC"))))

(add-hook 'magit-diff-mode-hook
          (lambda()
            (local-unset-key (kbd "SPC"))))

(require 'dired)
(add-hook 'dired-mode-hook
          (lambda()
            (local-unset-key (kbd "<normal-state> SPC"))))

(add-hook 'diff-hl-dired-mode-hook
          (lambda()
            (local-unset-key (kbd "SPC"))))

(add-hook 'all-the-icons-dired-mode-hook
          (lambda()
            (local-unset-key (kbd "SPC"))))


(use-package go-mode)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package typescript-mode)

(use-package npm)

(use-package dockerfile-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(use-package jenkinsfile-mode)
(use-package toml-mode)
(use-package protobuf-mode)
(use-package terraform-mode)
(use-package yaml-mode)
(add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.[jt]s[x]?\\'" . tsx-ts-mode))

(straight-use-package
 '(geist-font :type git :host github :repo "shaneikennedy/geist-font.el"))
(geist-font--install)
(ignore-errors (set-frame-font "Geist Mono-13"))

(use-package format-all)
(add-hook 'go-mode-hook 'format-all-mode)
(add-hook 'typescript-mode-hook 'format-all-mode)
(add-hook 'tsx-mode-hook 'format-all-mode)
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)

(setq treesit-language-source-alist
'(
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (css "https://github.com/tree-sitter/tree-sitter-css")))

(use-package yasnippet
  :ensure t
  :init
  ;; Enable YASnippet globally
  (yas-global-mode 1)
  :config
  ;; Set snippet directories
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-c y") 'yas-expand)

  ;; Integrate YASnippet with completion-at-point-functions
  (defun my/yas-capf ()
    "Add YASnippet completion to completion-at-point-functions with orderless support."
    (when (bound-and-true-p yas-minor-mode)
      (let ((completions (yas--get-snippets-as-capf)))
        (when completions
          (nconc completions '(:company-match #'orderless-try-completion))
          completions))))

  ;; Add YASnippet to completion-at-point-functions
  (add-hook 'completion-at-point-functions #'my/yas-capf nil t))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yasnippet-snippets-initialize))

(add-to-list 'load-path (expand-file-name "./lisp" user-emacs-directory))
(ignore-errors (require 'work))

;; Useful in monorepos
(add-to-list 'project-vc-extra-root-markers ".projectile")

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(diminish 'which-key-mode)
(diminish 'eldoc-mode)
(diminish 'evil-collection-unimpaired-mode)
(diminish 'dired-mode)
(diminish 'all-the-icons-dired-mode)
(diminish 'diff-hl-dired-mode)
(diminish 'rustic-doc-mode)
