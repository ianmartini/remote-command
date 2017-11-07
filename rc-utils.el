(defconst rc/messages "*rc/messages*")

(defun dos2unix (string) (replace-regexp-in-string "\r" "" string))

(defun rc/clear-messages ()
  (with-current-buffer (get-buffer-create rc/messages)
    (erase-buffer)))

(defun rc/message-impl (string)
  (with-current-buffer (get-buffer-create rc/messages)
    (save-excursion
      (goto-char (point-max))
      (insert (dos2unix string) "\n"))))

(defsubst _ts ()
  (format-time-string "%H:%M:%S" nil "GMT0"))

(defun rc/message (string &rest vars)
  (funcall #'rc/message-impl (apply #'format (cons (concat "[ " (_ts) " ] " string) vars))))

(defun rc/message-no-ts (string &rest vars)
  (funcall #'rc/message-impl (apply #'format (cons string vars))))

(provide 'rc-utils)
