;; Highlight selections (marks)
(setq transient-mark-mode t)

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(el-get 'sync)

(defun markdown-custom ()
  "markdown-mode-hook"
  (setq markdown-command "~/.cabal/bin/pandoc --smart --from=markdown --to=html5"))
(add-hook 'markdown-mode-hook '(lambda () (markdown-custom)))

(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

(setq vc-follow-symlinks t)

(setq ispell-program-name "aspell")
