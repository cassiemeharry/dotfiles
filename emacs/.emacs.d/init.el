;; Load org-mode to load main configuration
(package-initialize t)
(let ((custom-path-roots '("~/.emacs.d/init"
                           "~/.emacs.d/site-lisp"
                           "/usr/local/share/emacs/site-lisp")))
  (mapc (lambda (default-directory)
      (when (file-exists-p default-directory)
        (add-to-list 'load-path default-directory)
        (normal-top-level-add-subdirs-to-load-path)))
    custom-path-roots))

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

;; Start in *scratch* instead of the splash screen
(setq inhibit-splash-screen t)

;; Load remote files over SSH
(setq tramp-default-method "ssh")

;; Package Management
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))
(package-initialize)
(package-refresh-contents)
(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/init/nickmeharry.org") t)

(defun after-change-major-mode-hook ()
  "Sets the underscore character as a word boundary, so M-f and friends stop on it"
  (let* ((mode (symbol-name major-mode))
        (table (intern (concat mode "-syntax-table"))))
    (if (boundp table)
        (modify-syntax-entry ?_ "." (symbol-value table)))))

; Jinja2 mode
;   A templating language that's almost like the Django Template Language
;   Also good enough for HL7
(autoload 'jinja2-mode "jinja2-mode" "Major mode for the Jinja2 Template Language" t)
(add-to-list 'auto-mode-alist '("\\.hl7$" . jinja2-mode))

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

(global-set-key (kbd "H-[ h]") 'move-beginning-of-line)

; Fill and spell check git commit messages.
(add-to-list 'auto-minor-mode-alist '("COMMIT_EDITMSG" . auto-fill-mode))
(add-to-list 'auto-minor-mode-alist '("COMMIT_EDITMSG" . flyspell-mode))

; Sass
(add-to-list 'auto-mode-alist '("\\.sass" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.scss" . sass-mode))
(add-to-list 'auto-minor-mode-alist '("\\.sass" . rainbow-mode))
(add-to-list 'auto-minor-mode-alist '("\\.scss" . rainbow-mode))

(add-to-list 'auto-minor-mode-alist '("\\.exs?$" . alchemist-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-mode-type-alist (quote ((python-mode "python"))))
 '(ack-command "ack --nocolor --nogroup --nopager --")
 '(clean-buffer-list-delay-general 1)
 '(completion-ignored-extensions
   (quote
    (".beam" ".vee" ".jam" ".annot" ".cmi" ".cmxa" ".cma" ".cmx" ".cmo" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".byte" ".native")))
 '(custom-safe-themes
   (quote
    ("df96ff3758b4ffed868bd09046a7cbac239b88c0c125062b2f756adcc352d567" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
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
 '(idris-interpreter-path "/usr/local/bin/idris")
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
 '(org-hide-leading-stars t)
 '(org-highlight-sparse-tree-matches nil)
 '(org-level-color-stars-only t)
 '(org-refile-targets
   (quote
    (("newgtd.org" :maxlevel . 1)
     ("someday.org" :level . 2))))
 '(org-src-fontify-natively t)
 '(safe-local-variable-values
   (quote
    ((jedi:server-args "--virtual-env" "/Users/nick/.virtualenvs/drchrono-web")
     (jedi:server-args "--virtual-env" "/Users/nick/code/onpatient-web")
     (jedi:server-args "--virtual-env" "/Users/nick/code/drchrono-web"))))
 '(scroll-bar-mode nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(use-package-always-ensure t)
 '(vc-follow-symlinks t)
 '(visible-bell t)
 '(web-mode-markup-indent-offset 2)
 '(whitespace-global-modes (quote (not web-mode)))
 '(whitespace-style
   (quote
    (face tabs trailing space-before-tab space-after-tab tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :slant normal :weight normal :height 110 :width normal :family "Input Mono"))))
 '(agda2-highlight-coinductive-constructor-face ((t (:foreground "gold2"))))
 '(agda2-highlight-datatype-face ((t (:foreground "dodger blue"))))
 '(agda2-highlight-function-face ((t (:foreground "dodger blue"))))
 '(agda2-highlight-inductive-constructor-face ((t (:foreground "green2"))))
 '(agda2-highlight-module-face ((t (:foreground "medium purple"))))
 '(agda2-highlight-number-face ((t (:foreground "medium purple"))))
 '(agda2-highlight-postulate-face ((t (:foreground "dodger blue"))))
 '(agda2-highlight-primitive-face ((t (:foreground "dodger blue"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "dodger blue"))))
 '(agda2-highlight-record-face ((t (:foreground "dodger blue"))))
 '(diff-removed ((t (:foreground "red3"))))
 '(idris-loaded-region-face ((t nil)) t)
 '(idris-log-timestamp-face ((t (:foreground "Sky Blue-3" :weight bold))))
 '(idris-prover-processed-face ((t (:inverse-video t))))
 '(idris-semantic-type-face ((t (:foreground "Sky Blue-2"))))
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
