(in-package #:reform)

;; Messaging interface for replies, admin messages, broadcast messages etc

(defclass welcome-message (comment)
  ((welcome :initarg :welcome :initform nil :accessor welcome))
  (:metaclass ele:persistent-metaclass))


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

(defmethod print-message ((message welcome-message))
  (html (:h3 "Welcome to reform.ie!")
	(str (welcome message))))




(defun make-welcome-message ()
  (make-instance 'welcome-message
		 :welcome (html (:p "Your account has been created.
		 Please participate in
		 the " ((:a :href "/debates.html") "debates")
		 ", read the " ((:a :href "/articles.html") "articles")
		 " and propose ideas of your own."
		 (:br)
		 "We hope you enjoy the site!"
		 (:br)
		 "reform.ie"))
		 ))

