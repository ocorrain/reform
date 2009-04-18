(in-package #:reform)

(defclass user (reform-base-class)
  ((username :initform nil :initarg :username :accessor get-username :index t)
   (password :initform nil :initarg :password :accessor get-password)
   (email  :initarg :email :accessor get-email :initform nil)
   (capabilities :initform '() :accessor get-capabilities :index t)
   (liked :initform '() :accessor get-liked)
   (disliked :initform '() :accessor get-disliked))
  (:metaclass ele:persistent-metaclass))

(defun add-capability (user capability)
  (pushnew capability (get-capabilities user)))

(defun remove-capability (user capability)
  (with-slots (capabilities) user
    (setf (get-capabilities user)
	  (remove capability capabilities))))

(defun has-capability? (user capability)
  (let ((capabilities (get-capabilities user)))
    (or (member 'admin capabilities)
	(member capability capabilities))))

(defun has-capability* (capability)
  (and hunchentoot:*session*
       (hunchentoot:session-value 'user)
       (has-capability? (hunchentoot:session-value 'user) capability)))

(defun get-user-by-name (username)
  (ele:get-instance-by-value 'user 'username username))

(defun get-user ()
  (when (and (boundp 'hunchentoot:*session*)
	     hunchentoot:*session*) 
    (when-bind (user (hunchentoot:session-value 'user))
	       user)))


(defun maybe-get-user (username password)
  (when-bind (user (ele:get-instance-by-value 'user 'username username))
    (when (string-equal password (get-password user))
      user)))

(defun try-to-log-user-in (username password)
  (if-bind (user (maybe-get-user username password))
	   (progn (hunchentoot:start-session)
		  (setf (hunchentoot:session-value 'user) user))
	   (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)))

(defun user-pane (stream)
  (if-bind (user (hunchentoot:session-value 'user))
	   (with-html-output (s stream)
	     (:p (fmt "~A | " (get-username user))
		 ((:a :href "/logout.html") "log out")
		  (if (has-capability* 'admin)
		      (htm " | " ((:a :href "/new.html?type=user") "new user")
			   " | " ((:a :href "/edit-users.html") "edit users")))
		  (if (has-capability* 'poster)
		      (htm (:br) (fmt "Post new ~{~A~^, ~}."
				    (mapcar (lambda (type)
					      (with-html-output-to-string (s)
					       ((:a :href (url-rewrite:add-get-param-to-url "/new.html" "type" type))
						(str type))))
					    (list "article" "news" "debate" "tag")))) )))
	   (with-html-output (s stream)
	     ((:a :href "/login.html") "log in or register"))))

(hunchentoot:define-easy-handler (login-page :uri "/login.html"
					     :default-request-type :post)
    (username password)
  (when (and username password)
    (when (try-to-log-user-in username password)
      (hunchentoot:redirect "/")))
  (with-standard-page (:title "Log in to reform.ie")
    ((:div :class "span-12")
     ((:form :method "post" :enctype "multipart/form-data" :action "/login.html")
      (:fieldset
       (:legend "Log in")
       (:table
	(:tr (:td ((:label :for "username") "Username"))
	     (:td (:input :type "text" :name "username")))
	(:tr (:td ((:label :for "password") "Password"))
	     (:td (:input :type "password" :name "password")))))
      (:input :type "submit" :name "submit" :value "log in")))

    ((:div :class "span-12 last")
     (str (registration-form)))))

(defun registration-form ()
  (with-html-output-to-string (s)
    ((:form :method "post" :enctype "multipart/form-data" :action "/register.html")
      (:fieldset
       (:legend "Register")
       (:table
	(:tr (:td ((:label :for "username") "Username"))
	     (:td (:input :type "text" :name "username")))
	(:tr (:td ((:label :for "password") "Password"))
	     (:td (:input :type "password" :name "password")))
	(:tr (:td ((:label :for "password2") "Please type your password again"))
	     (:td (:input :type "password" :name "password2")))
	(:tr (:td ((:label :for "email") "Email (optional)"))
	     (:td (:input :type "text" :name "email")))))
     (:input :type "submit" :name "submit" :value "Register"))))

(hunchentoot:define-easy-handler (register :uri "/register.html")
    (username password password2 email)
  (let ((messages '()))
    (when (and username password password2)
      (if (get-user-by-name username)
	  (push "Username already taken.  Please choose another." messages)
	  (if (string-equal password password2)
	      (let ((user (make-instance 'user :username username :password password)))
		(when email (setf (get-email user) email))
		(try-to-log-user-in username password)
		(hunchentoot:redirect "/"))
	      (push "Passwords do not match" messages))))
    (with-standard-page (:title "Register")
      ((:div :class "span-12")
       (str (registration-form)))
      ((:div :class "span-12 last")
       (when messages
	 (htm ((:div :class "error")
	       (:ul (dolist (m messages) (htm (:li (str m))))))))))))


(hunchentoot:define-easy-handler (logout-page :uri "/logout.html")
    ()
  (when (and hunchentoot:*session*
	     (hunchentoot:session-value 'user))
    (hunchentoot:delete-session-value 'user)
    (hunchentoot:remove-session hunchentoot:*session*))
  (hunchentoot:redirect (hunchentoot:referer)))
