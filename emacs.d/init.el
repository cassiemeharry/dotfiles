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

;; Make C-a smarter
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Highlight selections (marks)
(setq transient-mark-mode t)
;; Highlight trailing spaces and all tabs
(global-whitespace-mode t)

;; Watch all files and revert buffers whose backing files have changed.
(global-auto-revert-mode t)

;; Don't clutter the filesystem with *~ files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory)))

(defun clean-backup-files ()
  (let ((week (* 60 60 24 7))
        (current (float-time (current-time))))
    (dolist (file (directory-files temporary-file-directory t))
      (when (and (backup-file-name-p file)
                 (> (- current (float-time (fifth (file-attributes file))))
                    week))
        (message "Deleting backup file %s" file)
        (delete-file file)))))

;; Cleanup whitespace
(defun cleanup-dir-whitespace (directory)
  "Clean up whitespace in all .py files in a given directory"
  (interactive "DDirectory to clean (recursively): ")
  (let ((py-files
         (f-entries "~/code/opw/onpatient-web"
                    (lambda (p) (s-ends-with? ".py" p))
                    t)))
    (mapcar (lambda (path)
              (message "Processing %s..." path)
              (with-temp-buffer
                (insert-file-contents path)
                (whitespace-cleanup)
                (write-region 1 (point-max) path)))
            py-files)
    (message "Done, cleaned %s files" (length py-files))))

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
  '(ack
    dash-at-point
    exec-path-from-shell
    elixir-mode elixir-mix
    flymake flymake-cursor flymake-jshint
    find-file-in-repository
    haste
    magit
    markdown-mode
    minimap
    multiple-cursors
    puppet-mode
    python-mode
    rainbow-mode
    rust-mode
    tuareg
    web-mode
    zencoding-mode
    s
    f
    jedi))
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

(defvar auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions, see `auto-mode-alist'
All elements of this alist are checked, meaning you can enable multiple minor modes for the same regexp.")
(defun enable-minor-mode-based-on-extension ()
  "check file name against auto-minor-mode-alist to enable minor modes
the checking happens for all pairs in auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name buffer-file-name)
          (remote-id (file-remote-p buffer-file-name))
          (alist auto-minor-mode-alist))
      ;; Remove backup-suffixes from file name.
      (setq name (file-name-sans-versions name))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(add-hook 'find-file-hook 'enable-minor-mode-based-on-extension)

;; Configure packages
(let* ((elpa-dir "~/.emacs.d/elpa/") ; Hack to load the correct python-mode
      (pm-dir (car (directory-files elpa-dir nil "python-mode-.+")))
      (python-mode.el (concat elpa-dir pm-dir "/python-mode.el")))
  (load python-mode.el))

(exec-path-from-shell-initialize)

(defun after-change-major-mode-hook ()
  "Sets the underscore character as a word boundary, so M-f and friends stop on it"
  (let* ((mode (symbol-name major-mode))
        (table (intern (concat mode "-syntax-table"))))
    (if (boundp table)
        (modify-syntax-entry ?_ "." (symbol-value table)))))

(add-to-list 'dash-at-point-mode-alist '(python-mode . "python2")) ; Uses Py3k by default
(when (eq system-type 'darwin)
  (global-set-key (kbd "C-c d") 'dash-at-point))

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

; Extra Python extensions
(add-to-list 'auto-mode-alist '("\\.wsgi$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.hl7t$" . python-mode))
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

; Open .asc files with EasyPG
(require 'epa-file)
(epa-file-enable)
(setq epa-file-name-regexp "\\.\\(gpg\\|\\asc\\)$")
(epa-file-name-regexp-update)

; Objective-C
(defun is-h-file-objc? ()
  (let* ((file-path (buffer-file-name))
         (file-ext (or (and (stringp file-path) (downcase (file-name-extension file-path))) ""))
         (m-file (concat (file-name-sans-extension file-path) ".m"))
         (mm-file (concat (file-name-sans-extension file-path) ".mm")))
    (and (string= file-ext "h")
         (or (file-exists-p m-file)
             (file-exists-p mm-file)
             (directory-files (file-name-directory file-path) t (regexp-opt '("\.xib" "\.xcodeproj")))))))
(add-to-list 'magic-mode-alist '(is-h-file-objc? . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm?$" . objc-mode))

; JS/Flymake
(require 'flymake-jshint)
;(add-hook 'javascript-mode-hook
;  (lambda () (flymake-mode t)))

(require 'multiple-cursors)
(global-set-key (kbd "M-N") 'mc/mark-next-like-this)
(global-set-key (kbd "M-P") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-SPC") 'mc/mark-all-like-this)

(setq vc-follow-symlinks t)

(require 'ido)
(ido-mode t)

(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Ensure ibuffer opens with point at the current buffer's entry.
(defadvice ibuffer
  (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name."
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

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
(defun flymake-ocaml-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-ocaml-cmdline))
(defun flymake-get-ocaml-cmdline (source base-dir)
  (list "~/bin/ocaml_flycheck.py" (list source base-dir)))
(add-to-list
 'flymake-allowed-file-name-masks
 '(".+\\.mli?"
   flymake-ocaml-init
   flymake-simple-cleanup
   flymake-get-real-file-name))
(add-hook
 'tuareg-mode-hook
 '(lambda ()
    (if (not (null buffer-file-name)) (flymake-mode))))
(let ((indent-versions (file-expand-wildcards "~/.opam/*/share/typerex/ocp-indent/ocp-indent.el"))
      (latest-file nil))
  (when indent-versions
    (with-temp-buffer
      (mapc (lambda (p) (insert p "\n")) indent-versions)
      (sort-lines nil (point-min) (point-max))
      (end-of-buffer)
      (delete-backward-char 1)
      (beginning-of-line)
      (setq latest-file (buffer-substring-no-properties (point) (point-max)))
      (load-file latest-file))))
(if (file-expand-wildcards "~/.opam/*/share/typerex/ocp-indent/ocp-indent.el"))

;; Magit, a Git frontend
(global-set-key (kbd "C-x g") 'magit-status)

;; Elixir, a Ruby-like language for the Erlang VM
(setq-default elixir-mode-map (make-keymap))
(autoload 'elixir-mode "elixir-mode" "Major mode for Elixir" t)
(add-to-list 'auto-mode-alist '("\\.exs?$" . elixir-mode))
(defun elixir-run-tests ()
  "Run the current buffer's project's Elixir Mix tests"
  (interactive)
  ; find mix.exs dir
  (let ((cwd (f-dirname buffer-file-name))
        (mix-dir (f-traverse-upwards (lambda (d)
                                       (member "mix.exs" (mapcar 'f-filename (f-files d)))))))
    (if (not mix-dir)
        (message "Could not find mix.exs")
      (shell-command (format "cd %S && mix test" mix-dir)))))
(define-key elixir-mode-map (kbd "C-c C-t") 'elixir-run-tests)

(autoload 'puppet-mode "puppet-mode" "Puppet config files" t)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

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

(if window-system
    (server-start))

(when (eq system-type 'darwin)
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))
  (setq interprogram-paste-function 'copy-from-osx)

  (defun paste-to-osx (text &optional push)
    (let* ((process-connection-type nil)
           (proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc)))
  (setq interprogram-cut-function 'paste-to-osx))

(defun sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file (as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-x C-r") 'sudo-edit)

(global-set-key (kbd "H-[ h]") 'move-beginning-of-line)

(defun single-window-with-minimap ()
  (interactive)
  (if (not (window-system))
      (message "Can not show minimap in terminal window")
    (delete-other-windows)
    (linum-mode nil)
    (minimap-create)
    (linum-mode t)))
(global-set-key (kbd "M-M") 'single-window-with-minimap)

; Fill and spell check git commit messages.
(add-to-list 'auto-minor-mode-alist '("COMMIT_EDITMSG" . auto-fill-mode))
(add-to-list 'auto-minor-mode-alist '("COMMIT_EDITMSG" . flyspell-mode))

; Sass
(add-to-list 'auto-mode-alist '("\\.sass" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scss" . sass-mode))
(add-to-list 'auto-minor-mode-alist '("\\.sass" . rainbow-mode))
(add-to-list 'auto-minor-mode-alist '("\\.scss" . rainbow-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clean-buffer-list-delay-general 1)
 '(delete-selection-mode t)
 '(electric-indent-mode nil)
 '(electric-pair-mode t)
 '(flymake-gui-warnings-enabled nil)
 '(flymake-no-changes-timeout 3)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-url-at-point t)
 '(indent-tabs-mode nil)
 '(initial-scratch-message "
")
 '(js-indent-level 2)
 '(midnight-hook (quote (clean-buffer-list clean-backup-files)))
 '(midnight-mode t nil (midnight))
 '(minimap-dedicated-window t)
 '(minimap-update-delay 0.1)
 '(minimap-window-location (quote right))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1)))
 '(tab-width 4)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(whitespace-global-modes (quote (not web-mode)))
 '(whitespace-style (quote (face tabs trailing space-before-tab space-after-tab tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-pre-face ((t (:inherit font-lock-constant-face :foreground "green")))))
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
