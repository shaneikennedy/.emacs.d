;;; init.el -- My Emacs configuration


;;; Commentary:
;; As of right now, common configurations are in ./modules
;; The code begins with setting up Emacs with use package and other necessary
;; initialization settings. It then loads my custom modules, and ends with misc
;; things that I can't find a good enough name to group together as


;;; Code:
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

;; Any Customize-based settings should live in custom.el, not here.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Keychain stuff. Note to self: if you keep having to enter your
;; keychain password on OS X, make sure that you have the following in .ssh/config:
;; Host *
;;    UseKeychain yes
(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; Ensure GNU ELPA has the GPG keys it needs
(use-package gnu-elpa-keyring-update)

(ignore-errors
  (set-frame-font "Hack Nerd Font Mono 13"))

;; Loading before nearly anything so than any package is diminishable and the modeline doesn't get fucked
(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode))

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Custom modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'my-gui)
(require 'my-editing)
(require 'my-python)
(require 'my-typescript)
;; (require 'my-haskell)
(require 'my-docker)
(require 'my-go)
;; (require 'my-cpp)
(require 'my-evil)
(require 'my-rust)
(require 'my-configs)
(require 'my-functions)
(require 'my-groovy)
(require 'my-java)

;;; MISC things
;; I do all of my writing in either org-mode or markdown-mode.
(use-package markdown-mode
  :mode ("\\.md$" . gfm-mode)
  :config
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown -t html")))
(use-package yaml-mode)
(use-package protobuf-mode)
(use-package jinja2-mode)
(add-to-list 'auto-mode-alist '("\\.avsc\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (rust-mode . rust-ts-mode)
        (go-mode . go-ts-mode)
        (python-mode . python-ts-mode)))

(provide 'init)
;;; init.el ends here
