(load-library "secrets.el.gpg")

(defun nm/get-secret (name)
  (let* ((name (if (symbolp name) (symbol-name name) name))
         (symbol (intern (concat "nm/secret/" name))))
    (eval symbol)))

(defun nm/apply-secret (name)
  (set (if (stringp name) (intern name) name)
       (nm/get-secret name)))

(provide 'secrets)
