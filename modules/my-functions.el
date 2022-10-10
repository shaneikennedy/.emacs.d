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

(defun bazel--run ()
  "Run Bazel command for a given directory."
  (interactive)
  (save-excursion
    (let* ((cmd (concat "bazelisk run " (bazel--get-relative-path))))
       (setq compilation-read-command t)
       (setq compile-command cmd)
       (set-buffer (find-file-noselect (projectile-project-root)))
       (call-interactively 'compile))))

(define-transient-command bazel--menu ()
  "Open bazel transient menu pop up."
    [["Bazel command"
      ("b" "Build"       bazel--build)
      ("r" "Run"         bazel--run)
      ("t" "Test"        bazel--test)]]
  (interactive)
  (transient-setup 'bazel--menu))

;; Optional
(add-hook 'java-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-b m") #'bazel--menu)
            (local-set-key (kbd "C-c C-b b") #'bazel--build)
            (local-set-key (kbd "C-c C-b r") #'bazel--run)
            (local-set-key (kbd "C-c C-b t") #'bazel--test)))

(add-hook 'bazel-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-b m") #'bazel--menu)
            (local-set-key (kbd "C-c C-b b") #'bazel--build)
            (local-set-key (kbd "C-c C-b r") #'bazel--run)
            (local-set-key (kbd "C-c C-b t") #'bazel--test)))

(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-b m") #'bazel--menu)
            (local-set-key (kbd "C-c C-b b") #'bazel--build)
            (local-set-key (kbd "C-c C-b r") #'bazel--run)
            (local-set-key (kbd "C-c C-b t") #'bazel--test)))

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-b m") #'bazel--menu)
            (local-set-key (kbd "C-c C-b b") #'bazel--build)
            (local-set-key (kbd "C-c C-b r") #'bazel--run)
            (local-set-key (kbd "C-c C-b t") #'bazel--test)))

(defun git-fetch-reset (remote)
  "Fetch REMOTE/master and reset master."
  (progn
    (magit-fetch-branch remote "master" ())
    (magit-branch-reset "master" (concat remote "/master"))))

(defun git-sync-origin ()
  "Sync branch with origin."
  (interactive)
  (git-fetch-reset "origin"))

(defun git-sync-upstream ()
  "Sync branch with origin."
  (interactive)
  (git-fetch-reset "upstream"))

(define-transient-command git-sync ()
  "Open git-sync transient menu pop up."
    [["Git sync"
      ("o" "origin"       git-sync-origin)
      ("u" "upstream"         git-sync-upstream)]]
  (interactive)
  (transient-setup 'git-sync))

(defun camelcase-region (start end)
  "Change region (START END) from snake_case to camelCase."
  (interactive "r")
  (save-restriction (narrow-to-region start end)
                    (goto-char (point-min))
                    (while (re-search-forward "_\\(.\\)" nil t)
                      (replace-match (upcase (match-string 1))))))

;; ----------------------------------------------------------------------
;; cadged largely from http://xahlee.org/emacs/elisp_idioms.html:
;;
(defun camelcase-word-or-region ()
  "Change word or region from snake_case to camelCase."
  (interactive)
  (let (pos1 pos2 bds)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))))
    (camelcase-region pos1 pos2)))

(provide 'my-functions)
;;; my-functions.el ends here
