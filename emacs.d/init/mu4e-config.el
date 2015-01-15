(require 'mu4e)
(require 'smtpmail)

(setq
  mu4e-maildir           "~/Maildir/drchrono_gmail"
  mu4e-sent-folder       "/Sent Mail"
  mu4e-drafts-folder     "/Drafts"
  mu4e-trash-folder      "/Trash"

  mu4e-sent-messages-behavior 'delete

  mu4e-get-mail-command  "offlineimap"
  mu4e-update-interval   300            ; Every 300 seconds
  mu4e-html2text-command "pandoc --from=html --to=markdown --columns=120"

  mu4e-hide-index-messages t
  mu4e-use-fancy-chars t

  mu4e-maildir-shortcuts
    '(("/INBOX"     . ?i)
      ("/Sent Mail" . ?s)
      ("/Trash"     . ?t)
      ("/All Mail"  . ?a))

  user-mail-address "nick@drchrono.com"
  user-full-name "Nick Meharry"
  message-signature
    (concat "Nick Meharry\n"
            "Software Developer\n"
            "drchrono\n")
  message-kill-buffer-on-exit t

  message-send-mail-function 'smtpmail-send-it
  starttls-use-gnutls t
  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
  smtpmail-auth-credentials '(("smtp.gmail.com" 587 "nick@drchrono.com" nil))
  smtpmail-smtp-server "smtp.gmail.com"
  smtp-smtp-service 587
)

(add-hook 'mu4e-index-updated-hook
          (defun notify-on-mail ()
            (start-process "terminal-notifier" nil
              "terminal-notifier"
              "-message" "M-x mu4e"
              "-title" "New mail as arrived"
              "-sound" "Pop"
              "-group" "mu4e-new-mail")))
