(defconst rc/re-match-line "^\\([^\n]*\\)\n")

(defvar rc/debug nil)
;; (setq rc/debug nil)

(defun rc/get-process (host)
  (get-process (concat "rc-" host)))

(defun rc/on-prompt (proc rc-data)
  (let ((pending-commands (rc/pending-commands rc-data)))
    (if (not pending-commands)
        (setf (rc/ready rc-data) t)
      (setf (rc/ready rc-data) nil)
      (funcall (pop (rc/pending-commands rc-data)) rc-data))))

(defun rc/filter (proc string)
  (let (rc-data filter-functions)
    (setq rc-data (get (intern (process-name proc)) 'rc-data))
    (setq filter-functions (rc/filter-functions rc-data))

    (condition-case err
        (progn
          (when rc/debug
            (rc/message "[debug/%s] %s"
                        (rc/host rc-data) (dos2unix string)))

          (cond (filter-functions
                 (funcall (car filter-functions) proc rc-data string))
                ;; ... match start-up error conditions?
                (t (rc/message string)))
          
          (when (and (not (rc/filter-functions rc-data))
                     (string-match (rc/prompt-regex rc-data) string))
            (rc/on-prompt proc rc-data)))

      ;; If the filter function fails, reset rc-data to new/ready

      (error
       (let ((host (rc/host rc-data)))
         (rc/reset-host-data host t)
         (message "[%s] (rc/filter ...) err -- [%s]" host err))))))

;; -- init

(defun rc/get-host-process (host)
  (interactive "sHost: ")
  (let (proc-name proc)
    (setq proc-name (concat "rc-" host))
    (setq proc (get-process proc-name))
    (unless proc
      ;; IAN - note ... bash doesn't echo anything on start-up which means
      ;;       that (rc/ready rc-data) is never set to t. FIXME please!
      (setq proc (start-process proc-name nil "bash"))
      (set-process-filter proc #'rc/filter)
      (rc/set-host-data host (rc/data-struct-new host proc)))
    proc))

;; remote commands

(defun rc/run-remote-command (rc-data command)
  (setf (rc/ready rc-data) nil)
  (funcall command rc-data))

(defun rc/add-pending-command (rc-data command)
  (push command (rc/pending-commands rc-data))
  (length (rc/pending-commands rc-data)))

(defun rc/send-remote-command (rc-data command)
  (if (rc/ready rc-data)
      (rc/run-remote-command rc-data command)
    (rc/add-pending-command rc-data command)))

(defun rc/process-command (host command per-line-fn at-end-fn skip-command)

  (rc/message "rc/process-command %s %s" host command)
  (rc/get-host-process host)

  (lexical-let ((command command)
                (per-line-fn per-line-fn)
                (at-end-fn at-end-fn)
                (skip-command skip-command)
                (previous-output ""))

    (rc/send-remote-command
     (rc/get-host-data host)

     ;; remote-command
     (lambda (rc-data)

       (rc/set-filter-functions
        (rc/host rc-data)

        ;; filter function
        (lambda (process rc-data string)
          (setq string (concat previous-output (dos2unix string)))
          (setq previous-output "")

          (let ((line nil)
                (pos 0))

            (while (string-match rc/re-match-line string pos)
              (setq line (match-string 1 string))
              (setq pos (match-end 0))

              ;; (rc/message "FIXME [%s] line: %s" host line)

              (if skip-command (setq skip-command nil)
                (when per-line-fn (funcall per-line-fn rc-data line))))

            (when (string-match (rc/prompt-regex rc-data) string)
              (when at-end-fn (funcall at-end-fn rc-data string))
              (setf (rc/filter-functions rc-data) nil))

            (when (< pos (length string))
              (setq previous-output (substring string pos))))))

       (process-send-string (rc/process rc-data) (concat command "\n"))))))

;; (rc/get-host-process "myhost")
;; (kill-process (rc/get-process "myhost"))

;; (setf (rc/ready (rc/get-host-data "myhost")) t)
;; (process-send-string (rc/get-host-process "myhost") "ls -l ; echo 'PROMPT> '\n")

(when t
  (rc/process-command "myhost" "ls -l ; echo 'PROMPT> '"
                      (lambda (rc-data line)
                        (rc/message "}}} %s {{{" line))
                      nil t))

(provide 'rc-infra)
