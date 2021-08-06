;;; my-rust.el -- Code for rust configuration

;;; Commentary:
;; Basic rust-mode setup

;;; Code:
(use-package rust-mode)

;; The Rust style guide recommends spaces rather than tabs for indentation
;; TAB should intend correctly for rust
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; I like format on save
(setq rust-format-on-save t)

;; Cargo commands run, build, test
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
(define-key rust-mode-map (kbd "C-c C-b") 'rust-build)
(define-key rust-mode-map (kbd "C-c C-t") 'rust-test)

;; LSP, duh.
(add-hook 'rust-mode-hook (lambda () (lsp)))

;; Rustlings
(defconst rustlings--root-cmd "rustlings ")

(defun rustlings--exec (command args)
 "Generic comilation for rustlings COMMAND ARGS."
  (save-excursion
    (let* ((cmd (concat rustlings--root-cmd command args)))
       (setq compilation-read-command t)
       (setq compile-command cmd)
       (set-buffer (find-file-noselect (projectile-project-root)))
       (call-interactively 'compile))))

(defun rustlings--current-exercise ()
  "Return the current exercise being worked on."
  (car (s-split "\\." (buffer-name))))

(defun rustlings--verify ()
  "Run Bazel's build for a given directory."
  (interactive)
  (rustlings--exec "verify" ""))

(defun rustlings--watch ()
  "Run Bazel's build for a given directory."
  (interactive)
  (rustlings--exec "watch" ""))

(defun rustlings--run (args)
  "Rustlings run for given ARGS."
  (interactive)
  (rustlings--exec "run " args))

(defun rustlings--run-current ()
  "Run the current exercise."
  (interactive)
  (rustlings--run (rustlings--current-exercise)))

(defun rustlings--run-next ()
  "Run the next exercise."
  (interactive)
  (rustlings--run "next"))

(defun rustlings--hint ()
  "Get the hint for your current exercise."
  (interactive)
  (rustlings--exec "hint " (rustlings--current-exercise)))

(define-transient-command rustlings--run-menu ()
  "Open bazel transient menu pop up."
    [["Rustlings command"
      ("n" "Next"       rustlings--run-next)
      ("c" "Current exercise"       rustlings--run-current)]]
  (interactive)
  (transient-setup 'rustlings--run-menu))


(define-transient-command rustlings--menu ()
  "Open bazel transient menu pop up."
    [["Rustlings command"
      ("v" "Verify"       rustlings--verify)
      ("w" "Watch"       rustlings--watch)
      ("h" "Hint"       rustlings--hint)
      ("r" "Run"       rustlings--run-menu)]]
  (interactive)
  (transient-setup 'rustlings--menu))

(provide 'my-rust)
;;; my-rust ends here
