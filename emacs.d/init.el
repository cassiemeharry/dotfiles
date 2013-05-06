(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/lang-modes")

;; Helper Functions
(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun all (predicate lst)
  "Return either 't or 'nil whether all items in a list pass a given predicate"
  (cond ((null lst) 't)
        ('t (if (funcall predicate (car lst))
                (all predicate (cdr lst))
              'nil))))

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

;; Other random keybinds
(global-set-key (kbd "C-x #") 'comment-region)

;; Highlight selections (marks)
(setq transient-mark-mode t)
;; Highlight trailing spaces and all tabs
(global-whitespace-mode t)

;; Watch all files and revert buffers whose backing files have changed.
(global-auto-revert-mode t)

;; Start in *scratch* instead of the splash screen
(setq inhibit-splash-screen t)

;; Cursor position helpers
(setq linum-format "%4d ")
(global-linum-mode t)
(column-number-mode t)
(setq scroll-error-top-bottom 'true)
(ignore-errors
  (load-theme 'tango-dark t))

;; Load remote files over SSH
(setq tramp-default-method "ssh")

;; Package Management
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
    '("melpa" .
      "http://melpa.milkbox.net/packages/"))
(package-initialize)
(defvar my-packages
  '(flymake flymake-cursor haste markdown-mode multiple-cursors python-mode rainbow-mode rust-mode tuareg zencoding-mode))
(defun my-packages-installed-p ()
  (all 'package-installed-p my-packages))

; Install packages not currently installed
(if (my-packages-installed-p)
    (message "All packages installed")
  (message "* Refreshing package database...")
  (package-refresh-contents)
  (message "* Done refreshing package database")
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (progn
        (message (format "  * Installing package %s..." (symbol-name p)))
        (package-install p)
        (message "  * Done installing package")))))

;; Configure packages
(let* ((elpa-dir "~/.emacs.d/elpa/") ; Hack to load the correct python-mode
      (pm-dir (car (directory-files elpa-dir nil "python-mode-.+")))
      (python-mode.el (concat elpa-dir pm-dir "/python-mode.el")))
  (load python-mode.el))

(defun after-change-major-mode-hook ()
  "Sets the underscore character as a word boundary, so M-f and friends stop on it"
  (let* ((mode (symbol-name major-mode))
        (table (intern (concat mode "-syntax-table"))))
    (if (boundp table)
        (modify-syntax-entry ?_ "." (symbol-value table)))))

(defun markdown-custom ()
  "markdown-mode-hook"
  (setq markdown-command "~/.cabal/bin/pandoc --smart --from=markdown --to=html5")
  (auto-fill-mode))
(if (file-exists-p "~/.cabal/bin/pandoc")
    (add-hook 'markdown-mode-hook '(lambda () (markdown-custom))))

; Rust mode
;   Rust is a new systems language from Mozilla.
;   http://www.rust-lang.org/
(autoload 'rust-mode "rust-mode" "Major mode for the Rust language" t)
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

; Jinja2 mode
;   A templating language that's almost like the Django Template Language
;   Also good enough for HL7
(autoload 'jinja2-mode "jinja2-mode" "Major mode for the Jinja2 Template Language" t)
(add-to-list 'auto-mode-alist '("\\.hl7$" . jinja2-mode))

; Web mode
;   Combines HTML, CSS, and JS modes in a reletively sane manner.
(autoload 'web-mode "web-mode" "Major mode for HTML5/JS/CSS" t)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.htm$" . web-mode))

; JS/Flymake
(when (load "flymake" t)
  (defun flymake-js-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "jsl" (list "-process" local-file "-nologo" "-nofilelisting" "-nocontext" "-nosummary"))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js" flymake-js-init)))
(add-hook 'find-file-hook
          (lambda ()
            (flymake-find-file-hook)
            (flymake-cursor-mode t)))

(require 'multiple-cursors)
(global-set-key (kbd "M-N") 'mc/mark-next-like-this)
(global-set-key (kbd "M-P") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-SPC") 'mc/mark-all-like-this)

(setq vc-follow-symlinks t)

(require 'ido)
(ido-mode t)

; Recent files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-c f") 'recentf-ido-find-file)

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

;; Flymake-OCaml
(require 'flymake)
(add-to-list
 'flymake-allowed-file-name-masks
 '(".+\\.mli?"
   flymake-simple-make-init
   flymake-simple-cleanup
   flymake-get-real-file-name))
(add-hook
 'tuareg-mode-hook
 '(lambda ()
    (if (not (null buffer-file-name)) (flymake-mode))))

(add-to-list 'load-path "~/.emacs.d/elixir-mode")
(setq-default elixir-mode-map (make-keymap))
(require 'elixir-mode)

(if (not window-system)
    (progn
      (require 'xterm-frobs)
      (defun my-xterm-title-hook ()
        (if (buffer-file-name)
            (let* ((filename (mapconcat 'identity (last (split-string (buffer-file-name) "/") 2) "/"))
                  (title
                   (concat
                    (cond (buffer-read-only "%  ")
                          ((buffer-modified-p) "*  "))
                    filename)))
              (progn
                (xterm-set-window-title title)
                (xterm-set-icon-title title)))))
      (add-hook 'post-command-hook  'my-xterm-title-hook))
  (setq frame-title-format
        '(""
          (:eval (cond (buffer-read-only "%%  ")
                       ((buffer-modified-p) "*  ")))
          "%b  (" invocation-name "@" system-name ")")))

(defun copy-to-clipboard
  (interactive))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode t)
 '(electric-indent-mode nil)
 '(electric-pair-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(indent-tabs-mode nil)
 '(initial-scratch-message "
")
 '(js-indent-level 2)
 '(tab-width 4)
 '(whitespace-style (quote (face tabs trailing space-before-tab empty space-after-tab tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-pre-face ((t (:inherit font-lock-constant-face :foreground "green")))))
