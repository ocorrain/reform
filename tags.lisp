(in-package #:reform)

(defun has-tag? (object tag-name)
  (member tag-name (get-tags object) :test #'equal))

(defun get-tag-by-name (tag-name)
  (ele:get-instance-by-value 'tag 'name tag-name))

(defun tag-toggle (object tag)
  (hunchentoot:log-message :info "Tagging ~A ~A ~A" (type-of object) (get-id object) (get-tag-name tag))
  (if (has-tag? object (get-tag-name tag))
      (setf (get-tags object) (remove (get-tag-name tag) (get-tags object) :test #'equal)
	    (get-tagged-objects tag) (remove object (get-tagged-objects tag)))
      (progn (push (get-tag-name tag) (get-tags object))
	     (push object (get-tagged-objects tag)))))

(defmethod print-tags-for-editing ((obj tagged-object-mixin) stream)
  (let ((all-tags (sort (copy-list (ele:get-instances-by-class 'tag))
			#'string< :key #'get-tag-name)))
    (let ((tag-links
	   (mapcar (lambda (tag)
		     (let* ((tag-name (get-tag-name tag))
			    (js-link (format nil "javascript:toggle(\"~A\",\"~A\",\"~A\")"
					     tag-name
					     (symbol-name (type-of obj))
					     (get-id obj))))
		       (with-html-output-to-string (s)
			 (if (has-tag? obj tag-name)
			     (htm ((:a :href "#" :onclick js-link :style "text-decoration: none; color:red;")
				   (str tag-name)))
			     (htm ((:a :href "#" :onclick js-link :style "text-decoration: none; color:black;")
				   (str tag-name)))))))
		   all-tags)))
      (format stream "Tags: ~{~A~^, ~}" tag-links))))

(defmethod print-tag-links ((obj tagged-object-mixin) stream)
  (if (get-tags obj)
      (with-html-output (s stream)
	((:h3 :class "alt") "Tags")
	(:ul (dolist (tag-name (get-tags obj))
	       (htm (:li ((:a :href (format nil "/display.html?type=tag&id=~A"
				       (get-id (get-tag-by-name tag-name))))
		     (str tag-name)))))))
      ""))

(defun get-tagged-of-type (tag type)
  (remove-if-not (lambda (obj) (equal type (type-of obj)))
		 (get-tagged-objects tag)))


(ht-ajax:defun-ajax toggle-tag (tag-name type instance-id) (*ajax-processor* :method :post)
	    (let ((real-id (parse-integer instance-id))
		  (class (get-valid-type type)))
	      (when (and real-id class)
		(when-bind (tag (get-tag-by-name tag-name))
			   (when-bind (object-to-tag (ele:get-instance-by-value class 'instance-id real-id))
				      (tag-toggle object-to-tag tag)
				      (with-output-to-string (s)
					(print-tags-for-editing object-to-tag s)
					s))))))
