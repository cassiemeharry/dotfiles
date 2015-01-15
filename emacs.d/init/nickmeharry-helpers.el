;; Helper functions
;; The kind of things emacs should have in the first place

(defun nm/chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (setq str (or str ""))
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

(defun nm/all (predicate lst)
  "Return either 't or 'nil whether all items in a list pass a given predicate"
  (cond ((null lst) 't)
        ('t (if (funcall predicate (car lst))
                (nm/all predicate (cdr lst))
              'nil))))

(defun nm/filter (predicate lst)
  "Filter out nil values after applying predicate to each item in the list"
  (delq nil
        (mapcar (lambda (item) (and (funcall predicate item) item)) lst)))

(defun nm/file-lines (file)
  "Read a file and return lines as a list."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n")))

(defun nm/file-first-line (file)
  "Read the first line from a file. Handy for passwords."
  (nm/chomp (or (car (nm/file-lines file)) "")))

(defun nm/pylint-ignore-unused-for-locals (&optional buffer)
  "Adds a pylint ignore line at the top of every function in buffer that uses"
  (interactive "bBuffer: ")
  (if (not buffer)
      (setq buffer (current-buffer)))
  (save-excursion
    (switch-to-buffer buffer)
    (goto-char (point-max))
    (while (search-backward "locals()" nil t)
      (search-backward "def ")
      (search-forward ":")
      (newline)
      (insert "# pylint: disable=unused-variable")
      (py-indent-line))))

(defun nm/js2-global-detection ()
  "Inform js2-mode about globals not declared in file, like $ or require"
  (when (> (buffer-size) 0)
    (let* ((btext (replace-regexp-in-string
                  ": *true" ""
                  (replace-regexp-in-string "[\n\t ]+" ""
                                            (buffer-substring-no-properties 1 (buffer-size)) t t)))
          (define-js-global (apply-partially 'add-to-list 'js2-additional-externs))
          (globals (append
                    (split-string
                     (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                                  " *, *" t)
                    '("require" "angular" "$"))))
      (mapc define-js-global globals))))
(add-hook 'js2-post-parse-callbacks 'nm/js2-global-detection)

(defun nm/rename-file-and-buffer ()
  "Rename the current buffer and the file it's visiting"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((vc-backend filename) (vc-rename-file filename new-name))
              (t (rename-file filename new-name t)
                 (set-visited-file-name new-name t t)))))))
(global-set-key (kbd "C-x C-r") 'nm/rename-file-and-buffer)

(require 'thingatpt)
(defun nm/drc/find-template-usage (template)
  "Finds all usage of the template. If called interactively, use
  filename like thing at point."
  (interactive
   (let ((filename-at-point (thing-at-point 'filename)))
     (when filename-at-point
       (setq filename-at-point (substring-no-properties filename-at-point))
       (let ((regexp ":[0-9]+:?$")      ; Strip off line and column numbers
             (before filename-at-point)
             same)
         (while (not same)
           (setq filename-at-point (replace-regexp-in-string regexp "" before)
                 same (string-equal before filename-at-point)
                 before filename-at-point))))
     (list (read-file-name "Template: "
                           (if (stringp filename-at-point)
                               (file-name-directory filename-at-point)
                             (concat (ack-and-a-half-read-dir) "templates/"))
                           nil t
                           (when (stringp filename-at-point)
                             (file-name-nondirectory filename-at-point))))))
  ;(setq template (replace-regexp-in-string "^.+templates/\\(v2\\)?" "" template))
;  (message "Would search for %S in %S" template (ack-and-a-half-read-dir)))
  (ack-and-a-half-run (ack-and-a-half-read-dir) nil (file-name-nondirectory template) "--python"))

(defun nm/markdown-mode/auto-fill-function ()
  (when (not (string-match "^\\(    |\t\\)" (thing-at-point 'line)))
    (funcall normal-auto-fill-function)))
(add-hook 'markdown-mode-hook
          (lambda ()
            (setq-local auto-fill-function 'nm/markdown-mode/auto-fill-function)))
