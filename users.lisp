(in-package #:reform)

(defclass user (reform-base-class)
  ((username :initarg :username :accessor get-username :index t)
   (password :initarg :password :accessor get-password)
   (email :initarg :email :accessor get-email :initform nil)
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
  (member capability (get-capabilities user)))

(defun has-capability* (capability)
  (and hunchentoot:*session*
       (hunchentoot:session-value 'user)
       (has-capability? (hunchentoot:session-value 'user) capability)))

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
	     (:hr :class "space")
	     (:p (fmt "Logged in as: ~A" (get-username user))
		 (fmt "(~{~A~^ ~})"
		      (mapcar (lambda (c)
				(string-downcase (symbol-name c)))
			      (get-capabilities user)))
		 (:br)
		 ((:a :href "/logout.html") "log out")
		 (if (has-capability* 'admin)
		     (htm " | " ((:a :href "/new.html?type=user") "new user")
			  " | " ((:a :href "/edit-users.html") "edit users")))))
	   (html-form 'login-form *standard-output*)))

(hunchentoot:define-easy-handler (login-page :uri "/login.html"
					     :default-request-type :post)
    (username password)
  (when (try-to-log-user-in username password)
    (hunchentoot:redirect (hunchentoot:referer))))

(hunchentoot:define-easy-handler (logout-page :uri "/logout.html")
    ()
  (when (and hunchentoot:*session*
	     (hunchentoot:session-value 'user))
    (hunchentoot:delete-session-value 'user)
    (hunchentoot:remove-session hunchentoot:*session*))
  (hunchentoot:redirect (hunchentoot:referer)))
