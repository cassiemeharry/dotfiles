;; Helper Functions
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun file-lines (file)
  "Read a file and return lines as a list."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n")))

(defun file-first-line (file)
  "Read the first line from a file. Handy for passwords."
  (chomp (car (file-lines file))))

;; Highlight selections (marks)
(setq transient-mark-mode t)

;; Watch all files and revert buffers whose backing files have changed.
(global-auto-revert-mode t)

;; el-get
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

; Install commonly used packages
(setq my-packages
  '(el-get color-theme flymake-cursor markdown-mode))
(el-get 'sync my-packages)

;; Configure packages
(defun markdown-custom ()
  "markdown-mode-hook"
  (setq markdown-command "~/.cabal/bin/pandoc --smart --from=markdown --to=html5"))
(add-hook 'markdown-mode-hook '(lambda () (markdown-custom)))

(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

(setq vc-follow-symlinks t)

(setq ispell-program-name "aspell")
(global-set-key (kbd "C-c f")
		(lambda ()
		  (interactive)
		  (ispell-change-dictionary "american")
		  (flyspell-mode 1)
		  (flyspell-buffer)))
(global-set-key (kbd "C-c F")
		(lambda ()
		  (interactive)
		  (ispell-change-dictionary "american")
		  (flyspell-prog-mode)))

(defun kill-all-buffers ()
  (interactive)
  "kill-all-buffers"
  (mapc 'kill-buffer (buffer-list)))

;; Python Debugger/Compile
(require 'python)
(require 'flymake)
(defun python--add-debug-highlight ()
  "Adds a highlighter for use by `python--pdb-breakpoint-string'"
  (highlight-lines-matching-regexp "pdb.set_trace\(\)" 'hi-red-b))

(add-hook 'python-mode-hook 'python--add-debug-highlight)

(defvar python--pdb-breakpoint-string "import pdb; pdb.set_trace()"
  "Python breakpoint string used by `python-insert-breakpoint'")

(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `pdb'"
  (interactive)
  (back-to-indentation)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (split-line)
  (insert python--pdb-breakpoint-string))
(define-key python-mode-map (kbd "<f5>") 'python-insert-breakpoint)

(defadvice compile (before ad-compile-smart activate)
  "Advises `compile' so it sets the argument COMINT to t
if breakpoints are present in `python-mode' files"
  (when (derived-mode-p major-mode 'python-mode)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (if (re-search-forward (concat "^\\s-*" python--pdb-breakpoint-string "$")
                               (point-max) t)
            ;; set COMINT argument to `t'.
            (ad-set-arg 1 t))))))

(when (load "flymake" t)
  (setq flymake-pylint-old-path nil)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name)))
	   (old-python-path (getenv "PYTHONPATH"))
	   (cmd-output (process-lines "~/.emacs.d/get-django-path.sh"))
	   )
      (flymake-log 3 "* PYTHONPATH script success? %s" (car cmd-output))
      (flymake-log 3 "* PYTHONPATH script output: %s" (car (cdr cmd-output)))
      (if (string= (car cmd-output) "Y")
	  (progn
	    (flymake-log 3 "setting PYTHONPATH from %s to %s" old-python-path (car (cdr cmd-output)))
	    (setq flymake-pylint-old-path old-python-path)
	    (setenv "PYTHONPATH" (car (cdr cmd-output))))
	(flymake-log 2 "** ERROR setting PYTHONPATH: %s" % (car (cdr cmd-output))))
      (list "epylint" (list buffer-file-name))))
  (defun flymake-pylint-cleanup ()
    (if flymake-pylint-old-path
        (progn
	  (flymake-log 3 "cleaning up temporary PYTHONPATH")
	  (setenv "PYTHONPATH" flymake-pylint-old-path)
	  (setq flymake-pylint-old-path nil))
      (setenv "PYTHONPATH" nil))
    (flymake-simple-cleanup))
  (add-to-list 'flymake-allowed-file-name-masks
    '("\\.py\\'" flymake-pylint-init flymake-pylint-cleanup)))

(eval-after-load "flymake"
  '(progn
     (defun flymake-after-change-function (start stop len)
       "Start syntax check for current buffer if it isn't already running."
       ;; Only run on save, as my pylint script takes too long and is synchronous.
       )))

(add-hook 'python-mode-hook
  (lambda () (flymake-mode t)))
