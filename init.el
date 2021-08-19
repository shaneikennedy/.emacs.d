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

(unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))

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

;; Custom modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'my-gui)
(require 'my-editing)
(require 'my-python)
(require 'my-javascript)
(require 'my-haskell)
(require 'my-docker)
(require 'my-go)
(require 'my-cpp)
(require 'my-evil)
(require 'my-rust)
(require 'my-configs)
(require 'my-functions)

;;; MISC things
;; I do all of my writing in either org-mode or markdown-mode.
(use-package markdown-mode
  :mode ("\\.md$" . gfm-mode)
  :config
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown -t html")))

(use-package dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
(use-package ivy-xref
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(provide 'init)
;;; init.el ends here
