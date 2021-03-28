;;; my-haskell -- Code for haskell configuration

;;; Commentary:

;;; Code:
(defmacro append-to-list (target suffix)
  "Append SUFFIX to TARGET in place."
  `(setq ,target (append ,target ,suffix)))

(use-package haskell-mode
  :config

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

  :mode ("\\.hs$" . haskell-mode))

(use-package hindent
  :ensure t)

(use-package flycheck-haskell
  :ensure t)

(use-package lsp-haskell
  :ensure t)

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
(add-hook 'haskell-mode-hook #'flycheck-haskell-setup)
(add-hook 'haskell-mode-hook #'hindent-mode)
(provide 'my-haskell)
;;; my-haskell.el ends here
