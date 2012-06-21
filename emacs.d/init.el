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

;; Grove.io IRC
; NickServ
(add-hook 'erc-after-connect
	  '(lambda (SERVER NICK)
	     (erc-message "PRIVMSG"
			  (concat "NickServ identify "
				  (file-first-line "~/.emacs.d/grove-irc-password.txt")))))))
; SSL
(require 'tls)
(setq tls-program
      '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
        "gnutls-cli --priority secure256 -p %p %h"))
; Grove
(defun connect-to-grove ()
  "Connect to Grove.io IRC"
  (interactive)
  (erc-tls :server "drchrono.irc.grove.io" :port 6697
           :nick "nickmeharry" :full-name "Nick Meharry")
  (setq erc-autojoin-channels-alist '(("#dev" "#drchrono" "#random"))))
