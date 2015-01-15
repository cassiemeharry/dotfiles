;; Load org-mode to load main configuration
(package-initialize t)
(add-to-list 'load-path "~/.emacs.d/init")
(add-to-list 'load-path "~/.emacs.d/lang-modes")

(load "nickmeharry-helpers")

;; Don't clutter the filesystem with *~ files
(setq
   backup-by-copying t  ; Don't clobber symlinks
   backup-directory-alist
     `((".*" . ,temporary-file-directory))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

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
  '(ack-and-a-half
    exec-path-from-shell
    elixir-mode elixir-mix
    flymake flymake-cursor flymake-jshint
    find-file-in-repository
    haste
    magit
    mo-git-blame
    markdown-mode
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
    smex
    xterm-frobs))

(defun my-packages-installed-p ()
  (nm/all 'package-installed-p my-packages))
(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
  are installed and are not in `jpk-packages'.  Useful for
  cleaning out unwanted packages."
  (interactive)
  (message "%s"
   (remove-if-not (lambda (x) (and (not (memq x my-packages))
                            (not (package-built-in-p x))
                            (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

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

(unless (eq system-type 'windows-nt)
  (exec-path-from-shell-initialize))

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

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; Extra Python extensions
(add-to-list 'auto-mode-alist '("\\.wsgi$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.hl7t$" . python-mode))

; Open .asc files with EasyPG
(require 'epa-file)
(epa-file-enable)
(setq epa-file-name-regexp "\\.\\(gpg\\|\\asc\\)$")
(epa-file-name-regexp-update)

; Objective-C
;;(defun is-h-file-objc? ()
;;   (let* ((file-path (buffer-file-name))
;;          (file-ext (or (and (stringp file-path) (downcase (file-name-extension file-path))) ""))
;;          (m-file (concat (file-name-sans-extension file-path) ".m"))
;;          (mm-file (concat (file-name-sans-extension file-path) ".mm")))
;;     (and (string= file-ext "h")
;;          (or (file-exists-p m-file)
;;              (file-exists-p mm-file)
;;              (directory-files (file-name-directory file-path) t (regexp-opt '("\.xib" "\.xcodeproj")))))))
;; nil)
;; (add-to-list 'magic-mode-alist '(is-h-file-objc? . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm?$" . objc-mode))

; JS/Flymake
(require 'flymake-jshint)
;(add-hook 'javascript-mode-hook
;  (lambda () (flymake-mode t)))

(setq vc-follow-symlinks t)

; Flyspell
;   Check my spelling while editing both text (C-c f) and code (C-c F).
(defun nm/autocorrect-prev-word ()
  "Autocorrects the previous word, wrapping in an undo marker if changed.
Meant to be bound to SPACE
"
  (interactive)
  (insert " ")
  (ispell-set-spellchecker-params)
  (ispell-accept-buffer-local-defs)
  (let ((cursor-location (point))
        (word (ispell-get-word nil))
        start end poss new-word replace)
    (setq start (car (cdr word))
          end (car (cdr (cdr word)))
          word (car word))
    (ispell-send-string "%\n") ; verbose mode
    (ispell-send-string (concat "^" word "\n"))
    (while (progn ; wait for ispell
             (ispell-accept-output)
             (not (string= "" (car ispell-filter)))))
    (setq ispell-filter (car (cdr ispell-filter))) ; remove extra \n
    (setq poss (ispell-parse-output ispell-filter))
    (when (and (listp poss) (not (cadddr poss)) (caddr poss))
      (setq new-word (caaddr poss))
      (message "Replacing %S with %S" word new-word)
      (delete-backward-char (- (marker-position end)
                               (marker-position start)
                               -1))
      (insert " ")
      (insert new-word))))
;; (add-hook 'flyspell-mode-hook (lambda ()
;;   (local-set-key (kbd "SPC") 'nm/autocorrect-prev-word)))
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

;; Magit, a Git frontend
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c g b") 'mo-git-blame-current)
(global-set-key (kbd "C-c g f") 'mo-git-blame-file)

;; (defun ansi-color-apply-on-buffer (&optional buffer)
;;   (interactive)
;;   (with-current-buffer (or buffer (current-buffer))
;;     (let ((original-bro buffer-read-only))
;;       (setq buffer-read-only nil)
;;       (ansi-color-apply-on-region (point-min) (point-max))
;;       (setq buffer-read-only original-bro))))

;; (defun ansify-magit-process-buffer ()
;;   (let ((buffer (car (nm/filter (lambda (b) (string-equal (buffer-name b) "*magit-process*"))
;;                              (buffer-list)))))
;;     (when buffer
;;       (save-excursion
;;         (set-buffer buffer)
;;         (make-local-variable 'after-change-functions)
;;         (add-hook 'after-change-functions
;;                   (lambda (start end length)
;;                     (ansi-color-apply-on-buffer (current-buffer))))))))
;; (add-hook 'buffer-list-update-hook 'ansify-magit-process-buffer)

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
; (global-set-key (kbd "C-x C-r") 'sudo-edit)

(global-set-key (kbd "H-[ h]") 'move-beginning-of-line)

; Fill and spell check git commit messages.
(add-to-list 'auto-minor-mode-alist '("COMMIT_EDITMSG" . auto-fill-mode))
(add-to-list 'auto-minor-mode-alist '("COMMIT_EDITMSG" . flyspell-mode))

; Sass
(add-to-list 'auto-mode-alist '("\\.sass" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scss" . sass-mode))
(add-to-list 'auto-minor-mode-alist '("\\.sass" . rainbow-mode))
(add-to-list 'auto-minor-mode-alist '("\\.scss" . rainbow-mode))

(add-to-list 'load-path "~/.emacs.d/repos/habitrpg")
(require 'habitrpg)
(setq habitrpg-api-user  "2e85d2f4-488d-45e7-bdff-f3f068491afe"
      habitrpg-api-token "d8303938-1d83-4ae1-937b-0ce0817b2117")
(global-set-key (kbd "C-c C-x h") 'habitrpg-add)
(global-set-key (kbd "<f9> a") 'habitrpg-status)

(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
                  (* 125 (frame-char-width)))
               2))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-mode-type-alist (quote ((python-mode "python"))))
 '(clean-buffer-list-delay-general 1)
 '(completion-ignored-extensions
   (quote
    (".beam" ".vee" ".jam" ".annot" ".cmi" ".cmxa" ".cma" ".cmx" ".cmo" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".byte" ".native")))
 '(custom-safe-themes
   (quote
    ("3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(delete-selection-mode t)
 '(electric-indent-mode nil)
 '(electric-pair-mode t)
 '(flymake-gui-warnings-enabled nil)
 '(flymake-no-changes-timeout 3)
 '(git-commit-summary-max-length 70)
 '(global-fci-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-url-at-point t)
 '(idris-interpreter-path "~/.cabal/bin/idris")
 '(indent-tabs-mode nil)
 '(initial-scratch-message "
")
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-strict-trailing-comma-warning nil)
 '(midnight-hook (quote (clean-buffer-list clean-backup-files)))
 '(midnight-mode t nil (midnight))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1)))
 '(safe-local-variable-values
   (quote
    ((encoding . utf-8)
     (jedi:server-args "--virtual-env" "/Users/nick/code/opw")
     (jedi:server-args "--virtual-env" "/Users/nick/code/dc-web"))))
 '(scroll-bar-mode nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(visible-bell t)
 '(whitespace-global-modes (quote (not web-mode)))
 '(whitespace-style
   (quote
    (face tabs trailing space-before-tab space-after-tab tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :background "#2e3436" :foreground "#eeeeec" :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Monaco"))))
 '(idris-loaded-region-face ((t nil)))
 '(idris-log-timestamp-face ((t (:foreground "Sky Blue-3" :weight bold))))
 '(idris-prover-processed-face ((t (:inverse-video t))))
 '(idris-semantic-type-face ((t (:foreground "Sky Blue-2"))))
 '(magit-diff-add ((t (:inherit diff-added :foreground "#729fcf"))))
 '(magit-item-highlight ((t (:inverse-video nil :box nil :slant oblique :weight extra-bold))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face :foreground "green"))))
 '(term-bold ((t (:weight extra-bold))))
 '(term-color-black ((t (:background "Aluminium-6" :foreground "Aluminium-4"))))
 '(term-color-blue ((t (:background "Sky Blue-3" :foreground "Sky Blue-1"))))
 '(term-color-cyan ((t (:background "cyan3" :foreground "light cyan"))))
 '(term-color-green ((t (:background "Chameleon-3" :foreground "Chameleon-2"))))
 '(term-color-magenta ((t (:background "Plum-3" :foreground "Plum-1"))))
 '(term-color-red ((t (:background "Scarlet Red-3" :foreground "Scarlet Red-1"))))
 '(term-color-white ((t (:background "Aluminium-2" :foreground "Aluminium-1"))))
 '(term-color-yellow ((t (:background "Butter-3" :foreground "Butter-2")))))
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/init/nickmeharry.org"))
