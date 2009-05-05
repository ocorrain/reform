(in-package #:reform)

;; Messaging interface for replies, admin messages, broadcast messages etc

(hunchentoot:define-easy-handler (show-messages :uri "/messages")
    ()
  (if-bind (user (get-user))
	   (with-standard-page (:ajax t :title (format nil "Messages for ~A" (get-username user)))
	     (when-bind (new-messages (get-messages user))
			(htm (:h3 "New messages")
			     (dolist (m (get-messages user))
			       (htm (str (print-message m))))
			      (:hr)))
	     (dolist (m (get-old-messages user))
		(htm (str (print-message m))))
	     (setf (get-old-messages user) (append (get-messages user)
						   (get-old-messages user))
		   (get-messages user) nil))
	   (hunchentoot:redirect "/")))

(defmethod print-message ((message comment))
  (html (:p ((:a :href (get-url (get-debate message)))
	     (str (get-title (get-debate message))))
	    (str (print-comment message 0 t)))))




