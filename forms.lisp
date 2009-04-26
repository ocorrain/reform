(in-package #:reform)

(defmethod html-form :around ((object post) output-stream &optional new)
  (with-html-output (s output-stream :indent t)
    ((:form :method "post" :enctype "multipart/form-data" :action "/edit.html")
     (call-next-method object output-stream)
     (:fieldset
      (:legend "Publishing information")
      (if (published? object)
	  (htm (:input :name "published" :type "checkbox" :value "published"
		       :checked "yes"))
	  (htm (:input :name "published" :type "checkbox" :value "published")))
      "Published"
      (if (sticky? object)
	  (htm (:input :name "sticky" :type "checkbox" :value "sticky"
		       :checked "yes"))
	  (htm (:input :name "sticky" :type "checkbox" :value "sticky")))
      "Sticky"
      (:input :name "instance-id" :type "hidden" :value (get-id object))
      (:input :name "type" :type "hidden" :value (type-of object))
      (:input :name "edit" :type "hidden" :value "yes"))
     
     (:input :type "reset" :name "reset" :value "Reset")
     (:input :type "submit" :name "submit" :value "Save")
     (:input :type "submit" :name "display" :value "Finish"))))

(defmethod html-form ((object tag) output-stream &optional new)
  (with-html-output (s output-stream :indent t)
    (:fieldset
     (:legend "Tag")
     (:table
      (:tr (:td ((:label :for "name") "Name"))
	   (:td (:input :name "name" :type "text" :id "name"
			:value (esc (or (get-tag-name object) "")))))
      (:tr (:td ((:label :for "rubric") "Rubric")
		(:p "Optional exposition of the theme"))
	   (:td ((:textarea :name "rubric" :rows 10 :cols 62)
		 (str (or (get-rubric object) "")))))))))

(defmethod html-form ((object news) output-stream &optional new)
  (with-html-output (s output-stream :indent t)
    (:fieldset
     (:legend "News item")
     (:table
      (:tr (:td ((:label :for "headline") "Headline"))
	   (:td (:input :name "headline" :type "text" :id "headline"
			:value (esc (or (get-headline object) "")))))
      (:tr (:td ((:label :for "author") "Author"))
	   (:td (:input :name "author" :type "text" :id "author"
			:value (esc (or (get-author object) ""))))))
     ((:textarea :name "story" :rows 10 :cols 62)
      (if-bind (story (get-story object))
	       (htm (str story))
	       "") ))))

;; (defmethod html-form ((object (eql 'comment)) output-stream)
;;   (with-html-output (s output-stream :indent t)
;;     ((:form :method "post" :action "/post-comment.html")
;;      (:textarea :style "width: 400px; height: 100px"
;; 				  :name "comment" :rows 5 :cols 20) (:br)
;;      (:input :type "radio" :name "motion" :value "for" :checked "checked") "For" 
;; 		       (:input :type "radio" :name "in-favour" :value "against") "Against" 
;; 		       ((:button :type "submit") "comment"))))

(defgeneric comment-form (obj))

(defmethod comment-form ((debate debate))
  (with-html-output-to-string (s)
    ((:form :method "post" :action "/post-comment.html")
     (:textarea :style "width: 500px; height: 75px"
		:name "comment" :rows 5 :cols 20) (:br)
     ((:button :type "submit" :name "vote" :value "for") "vote for")
     ((:button :type "submit" :name "vote":value "against") "vote against")
     (:input :type "hidden" :name "debate" :value (get-id debate)))))

(defmethod comment-form ((obj news))
  (with-html-output-to-string (s)
    ((:form :method "post" :action "/post-to-thread.html")
     (:textarea :style "width: 500px; height: 75px"
		:name "comment" :rows 5) (:br)
     ((:button :type "submit" :name "comment") "comment")
     (:input :type "hidden" :name "reply-to" :value (get-id obj))
     (:input :type "hidden" :name "type" :value (type-of obj)))))


(defmethod html-form ((object article) output-stream &optional new)
  (with-html-output (s output-stream :indent t)
    (:fieldset
     (:legend "News item")
     (:table
      (:tr (:td ((:label :for "headline") "Title"))
	   (:td (:input :type "text" :name "headline"
			:value (esc (or (get-headline object) "")))))
      (:tr (:td ((:label :for "author") "Author"))
	   (:td (:input :type "text" :name "author"
			:value (esc (or (get-author object) "")))))
      (:tr (:td ((:label :for "upload") "Upload")
		(:p "Only HTML and plain text files"))
	   (:td (:input :type "file" :name "upload")))))))


(defmethod html-form ((object debate) output-stream &optional new)
  (with-html-output (s output-stream :indent t)
    (:fieldset
     (:legend "Debate")
     (:table
      (:tr (:td ((:label :for "motion") "Motion"))
	   (:td (:input :type "text" :name "motion" 
			:value (esc (or (get-motion object) "")))))
      (:tr (:td ((:label :for "rubric") "Rubric")
		(:p "Optional exposition of the theme"))
	   (:td ((:textarea :name "rubric" :rows 10 :cols 62)
		 (str (or (get-rubric object) "")))))))))

(defparameter *capabilities* '(admin poster moderator))

(defmethod html-form ((object user) output-stream &optional new)
  (with-html-output (s output-stream :indent t)
    ((:form :method "post" :enctype "multipart/form-data" :action "/edit.html")
     (:fieldset
      (:legend "User information")
      (:table
       (:tr (:td ((:label :for "username") "Username"))
	    (:td (:input :type "text" :name "username" :value (or (get-username object) ""))))
       (:tr (:td ((:label :for "password") "Password"))
	    (:td (:input :type "password" :name "password")))
       (:tr (:td ((:label :for "password2") "Please type your password again"))
	    (:td (:input :type "password" :name "password2")))
       (:tr (:td ((:label :for "email") "Email (optional)"))
	    (:td (:input :type "text" :name "email" :value (or (get-email object) "")))))
      (when (has-capability* 'admin))
       (write-capability-checkboxes *capabilities* object s))
     (:input :name "instance-id" :type "hidden" :value (get-id object))
     (:input :name "type" :type "hidden" :value (type-of object))
     (:input :name "edit" :type "hidden" :value "yes")
     (when new
       (htm (:input :name "new" :type "hidden" :value "yes")))
     (:input :type "reset" :name "reset" :value "Reset")
     (:input :type "submit" :name "submit" :value "Save")
     (:input :type "submit" :name "display" :value "Finish"))))


(defun write-capability-checkboxes (capabilities user stream)
  (with-html-output (s stream :indent t)
    (:table (dolist (c capabilities)
	      (let ((stringy-c (string-downcase (symbol-name c))))
		(htm (:tr (:td (str stringy-c))
			  (:td (if (has-capability? user c)
				   (htm (:input :name stringy-c :type "checkbox" :value stringy-c
						:checked "checked"))
				   (htm (:input :name stringy-c :type "checkbox" :value stringy-c)))))))))))




(defmethod html-form ((object (eql 'login-form)) output-stream &optional new)
  (with-html-output (s output-stream :indent t)
    (:hr :class "space")
    ((:form :method "post" :action "/login.html")
     "user" (:input :type "text" :size 5 :name "username") 
     "pass" (:input :type "password" :size 5 :name "password")
     ((:button :type "submit") "login"))))

(defun whitespace-char-p (char)
  (member char (list #\Space #\Tab #\Return #\Newline)))

(defun meaningful-string (string)
  (when (and (> (length string) 0)
	     (not (every #'whitespace-char-p string)))
    string))


(defun get-form-field (symb parameters)
  (let ((val (cdr (assoc symb parameters))))
    (when val
      (hunchentoot:log-message :info "XX~SXX" val)
      (cond ((stringp val) (meaningful-string val))
	    (t val)))))
 
(defmethod apply-changes :before ((obj post) parameters)
  (setf (sticky? obj)
	(if (get-form-field 'sticky parameters) t nil))
  (setf (published? obj)
	(if (get-form-field 'published parameters) t nil)))

(defmethod apply-changes ((obj tag) parameters)
  (when-bind (n (get-form-field 'name parameters))
	     (setf (get-tag-name obj) n))
  (when-bind (r (get-form-field 'rubric parameters))
	     (setf (get-rubric obj) r)))

(defmethod apply-changes ((obj news) parameters)
  (when-bind (h (get-form-field 'headline parameters))
    (setf (get-headline obj) h))
  (when-bind (s (get-form-field 'story parameters))
    (setf (get-story obj) s))
  (when-bind (a (get-form-field 'author parameters))
    (setf (get-author obj) a)))

(defparameter *acceptable-content-types*
  '("application/xhtml+xml" "text/html" "text/plain"))

(defmethod apply-changes ((obj article) parameters)
  (when-bind (title (get-form-field 'headline parameters))
    (setf (get-headline obj) title))
  (when-bind (author (get-form-field 'author parameters))
    (setf (get-author obj) author))
  (when-bind (upload (get-form-field 'upload parameters))
    (destructuring-bind (path filename content-type)
	upload
      (declare (ignore filename))
      (if (member content-type *acceptable-content-types* :test #'equal)
	  (setf (get-story obj) (html-clean path))
	  (error "Unacceptable content")))))

(defmethod apply-changes ((obj debate) parameters)
  (when-bind (motion (get-form-field 'motion parameters))
    (setf (get-motion obj) motion))
  (when-bind (rubric (get-form-field 'rubric parameters))
    (setf (get-rubric obj) rubric)))

(defmethod apply-changes ((obj user) parameters)
  (let ((messages '())
	(info '()))
    (when-bind (p (get-form-field 'password parameters))
	       (when-bind (p2 (get-form-field 'password2 parameters))
			  (if (string-equal p p2)
			      (progn
				(setf (get-password obj) p)
				(push "Password changed" info))
			      (push "Passwords do not match" messages))))
    (when-bind (u (get-form-field 'username parameters))
	       (if (get-form-field 'new parameters)
		   (if (get-user-by-name u)
		       (push "This username is already taken, please choose another." messages)
		       (setf (get-username obj) u))
		   (if-bind (ou (get-user-by-name u))
			    (if (/= (get-id obj) (get-id ou))
				(push "This username is already taken, please choose another." messages))
			    (setf (get-username obj) u))))
    (when-bind (e (get-form-field 'email parameters))
	       (setf (get-email obj) e))
    (dolist (c *capabilities*)
      (if (get-form-field c parameters)
	  (add-capability obj c)
	  (remove-capability obj c)))
    (values obj messages info)))

