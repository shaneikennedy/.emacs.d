;;; my-functions.el -- Dumping ground for my random elisp.

;;; Commentary:

;;; Code
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
