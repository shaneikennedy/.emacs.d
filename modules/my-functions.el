;;; my-functions.el -- Dumping ground for my random elisp.

;;; Commentary:

;;; Code:
(defun sk/apply-macro-page ()
  "Apply the currently defined keyboard marco to everyline of the page."
  (interactive)
  (if (< (line-number-at-pos) (count-screen-lines))
      (progn
          (command-execute 'call-last-kbd-macro)
          (forward-line)
          (sk/apply-macro-page))
      (progn
          (command-execute 'call-last-kbd-macro)
          (message "Macro applied to full page."))))


(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun workon-local-package ()
  "Add the package that I'm working on to my load path."
  (interactive)
  (add-to-list 'load-path (projectile-project-root)))

(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  "Make sure compilation buffers show colors."
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

(defun bazel--get-relative-path ()
  "Get the current file path relative to projectiile's root."
  (concat "//" (string-remove-prefix (projectile-project-root) (file-name-directory (buffer-file-name))) "..."))

(defun bazel--build ()
  "Run Bazel's build for a given directory."
  (interactive)
  (save-excursion
    (let* ((cmd (concat "bazelisk build " (bazel--get-relative-path))))
       (setq compilation-read-command t)
       (setq compile-command cmd)
       (set-buffer (find-file-noselect (projectile-project-root)))
       (call-interactively 'compile))))

(defun bazel--test ()
  "Run Bazel's test for a given directory."
  (interactive)
  (save-excursion
    (let* ((cmd (concat "bazelisk test " (bazel--get-relative-path))))
       (setq compilation-read-command t)
       (setq compile-command cmd)
       (set-buffer (find-file-noselect (projectile-project-root)))
       (call-interactively 'compile))))

(define-transient-command bazel--menu ()
  "Open bazel transient menu pop up."
    [["Bazel command"
      ("b" "Build"       bazel--build)
      ("t" "Test"       bazel--test)]]
  (interactive)
  (transient-setup 'bazel--menu))

;; Optional
(add-hook 'java-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-b m") #'bazel--menu)
            (local-set-key (kbd "C-c C-b b") #'bazel--build)
            (local-set-key (kbd "C-c C-b t") #'bazel--test)))

(provide 'my-functions)
;;; my-functions.el ends here
