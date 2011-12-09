;; Add directories to load-path
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(setq
 el-get-sources
 '(el-get
   ack
   anything
   apache-mode
   coffee-mode
   haskell-mode
   php-mode
   python-pep8
   smart-tab
   smooth-scrolling
   tail
   zencoding-mode))

(el-get 'sync)

(require 'less-mode)

(global-set-key "\M-/" 'dabbrev-expand-multiple)

(setq uniquify-buffer-name-style 'reverse)

(defun smart-split ()
  "Split the frame into 110-column sub-windows, and make sure no window has
   fewer than 110 columns."
  (interactive)
  (defun smart-split-helper (w)
    "Helper function to split a given window into two, the first of which has 
     110 columns."
    (if (> (window-width w) (* 2 111))
    (let ((w2 (split-window w 112 t)))
      (smart-split-helper w2))))
  (smart-split-helper nil))

(smart-split)

(column-number-mode t)

(setf user-mail-address "bluejeansummer@gmail.com")

(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

(line-number-mode nil)
(setq linum-format "%4d ")
(global-linum-mode t)
