(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/lang-modes")

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

;; Disable backgrounding emacs with C-z.
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Highlight selections (marks)
(setq transient-mark-mode t)

;; Watch all files and revert buffers whose backing files have changed.
(global-auto-revert-mode t)

;; Cursor position helpers
(setq linum-format "%4d ")
(global-linum-mode t)
(column-number-mode t)
(setq scroll-error-top-bottom 'true)

;; Package Management
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)
(defvar my-packages ; color-theme and color-theme-tango should be in this list as well
  '(flymake flymake-cursor haste markdown-mode rust-mode))
(defun my-packages-installed-p ()
  (let (all-installed 't)
    (dolist (p my-packages)
      (if (not (package-installed-p p))
	  (setq all-installed nil)))
    all-installed))
; Install packages not currently installed
(unless (my-packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" "done.")
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Configure packages
(defun markdown-custom ()
  "markdown-mode-hook"
  (setq markdown-command "~/.cabal/bin/pandoc --smart --from=markdown --to=html5")
  (auto-fill-mode))
(add-hook 'markdown-mode-hook '(lambda () (markdown-custom)))

; Rust mode
;   Rust is a new systems language from Mozilla.
;   http://www.rust-lang.org/
(autoload 'rust-mode "rust-mode" "Major mode for the Rust language" t)
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

; Web mode
;   Combines HTML, CSS, and JS modes in a reletively sane manner.
(autoload 'web-mode "web-mode" "Major mode for HTML5/JS/CSS" t)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.htm$" . web-mode))

(load-theme 'tango-dark t)

(setq vc-follow-symlinks t)

; Flyspell
;   Check my spelling while editing both text (C-c f) and code (C-c F).
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
		  (flyspell-prog-mode)
		  (flyspell-buffer)))

;; Python Debugger/Compile
(autoload 'python-mode "python" "Major mode for the Python language" t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
;; (eval-after-load "python"
;;   '(
;;     (defvar python--pdb-breakpoint-string "import pdb; pdb.set_trace()"
;;       "Python breakpoint string used by `python-insert-breakpoint'")
;;     (add-hook 'python-mode-hook
;;               (lambda () (highlight-lines-matching-regexp (regexp-quote python--pdb-breakpoint-string) 'hi-red-b)))
;;     (defun python-insert-breakpoint ()
;;       "Inserts a python breakpoint using `pdb'"
;;       (interactive)
;;       (back-to-indentation)
;;       ;; this preserves the correct indentation in case the line above
;;       ;; point is a nested block
;;       (split-line)
;;       (insert python--pdb-breakpoint-string))
;;     (define-key python-mode-map (kbd "<f5>") 'python-insert-breakpoint)

;;     (require 'flymake)
;;     (add-hook 'python-mode-hook
;;               (lambda () (flymake-mode t)))

;;     (defadvice compile (before ad-compile-smart activate)
;;       "Advises `compile' so it sets the argument COMINT to t
;;       if breakpoints are present in `python-mode' files"
;;       (when (derived-mode-p major-mode 'python-mode)
;;         (save-excursion
;;           (save-match-data
;;             (goto-char (point-min))
;;             (if (re-search-forward (concat "^\\s-*" python--pdb-breakpoint-string "$")
;;                                    (point-max) t)
;;                 ;; set COMINT argument to `t'.
;;                 (ad-set-arg 1 t))))))

;;     (when (load "flymake" t)
;;       (setq flymake-pylint-old-path nil)
;;       (defun flymake-pylint-init ()
;;         (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                            'flymake-create-temp-inplace))
;;                (local-file (file-relative-name
;;                             temp-file
;;                             (file-name-directory buffer-file-name)))
;;                (old-python-path (getenv "PYTHONPATH"))
;;                (cmd-output (process-lines "~/.emacs.d/get-django-path.sh"))
;;                )
;;           (flymake-log 3 "* PYTHONPATH script success? %s" (car cmd-output))
;;           (flymake-log 3 "* PYTHONPATH script output: %s" (car (cdr cmd-output)))
;;           (if (string= (car cmd-output) "Y")
;;               (progn
;;                 (flymake-log 3 "setting PYTHONPATH from %s to %s" old-python-path (car (cdr cmd-output)))
;;                 (setq flymake-pylint-old-path old-python-path)
;;                 (setenv "PYTHONPATH" (car (cdr cmd-output))))
;;             (flymake-log 2 "** ERROR setting PYTHONPATH: %s" (car (cdr cmd-output))))
;;           (list "epylint" (list buffer-file-name))))
;;       (defun flymake-pylint-cleanup ()
;;         (if flymake-pylint-old-path
;;             (progn
;;               (flymake-log 3 "cleaning up temporary PYTHONPATH")
;;               (setenv "PYTHONPATH" flymake-pylint-old-path)
;;               (setq flymake-pylint-old-path nil))
;;           (setenv "PYTHONPATH" nil))
;;         (flymake-simple-cleanup))
;;       (add-to-list 'flymake-allowed-file-name-masks
;;                    '("\\.py\\'" flymake-pylint-init flymake-pylint-cleanup)))))

;; (eval-after-load "flymake"
;;   '(progn
;;      (defun flymake-after-change-function (start stop len)
;;        "Start syntax check for current buffer if it isn't already running."
;;        ;; Only run on save, as my pylint script takes too long and is synchronous.
;;        )))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
