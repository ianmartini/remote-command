(defstruct (rc/data-struct (:conc-name rc/))
  host
  process
  prompt-regex
  ready
  filter-functions
  pending-commands
  extra)

(defun rc/data-struct-new (host process)
  (let ((var (make-rc/data-struct)))
    (setf (rc/host var) host)
    (setf (rc/process var) process)
    (setf (rc/prompt-regex var) "PROMPT> ")
    var))

(defun rc/get-host-process (host)
  (get-process (concat "rc-" host)))

(defsubst rc/set-host-data (host rc-data)
  (put (intern (concat "rc-" host)) 'rc-data rc-data))

(defsubst rc/get-host-data (host)
  (get (intern (concat "rc-" host)) 'rc-data))

(defun rc/reset-host-data (host &rest ready)
  (let ((rc-data (rc/data-struct-new host (rc/get-host-process host))))
    (setf (rc/ready rc-data) (if ready (car ready) nil))
    (rc/set-host-data host rc-data))
  nil)

(defun rc/set-filter-functions (host &rest functions)
  (setf (rc/filter-functions (rc/get-host-data host))
        (if (car functions) functions nil)))

(provide 'rc-types)
