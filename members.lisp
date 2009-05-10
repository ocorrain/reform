(in-package #:reform)

;; members' only section

(hunchentoot:define-easy-handler (contacts :uri "/contacts")
    ((instance-id :parameter-type 'integer))
  ;; (when-bind (parameter (hunchentoot:post-parameters*))
  ;; 	     (if-bind (obj (ele:get-instance-by-value 'comment 'instance-id instance-id))
  ;; 		      (apply-changes obj (regularize-alist (hunchentoot:post-parameters)))
  ;; 		      (apply-changes (make-instance 'contact (regularize-alist (hunchentoot:post-parameters))))))
  
  (let ((static (alexandria:read-file-into-string
		 (merge-pathnames "press.html" (get-config-option :static-dir)))))
    (with-standard-page (:title "Directory of contacts" :ajax t)
      (setf (hunchentoot:header-out :cache-control) "no-cache, must-revalidate")
      (htm ((:div :class "span-24")
	    (str static))))))


