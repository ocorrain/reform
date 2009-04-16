(in-package #:reform)

(defun create-handlers ()
  (setq hunchentoot:*dispatch-table*
	(list 'hunchentoot:dispatch-easy-handlers
	      (hunchentoot:create-folder-dispatcher-and-handler
	       "/css/" (make-pathname
			:directory '(:absolute "home" "reform" "src"
				     "blueprint-css")))
	      (hunchentoot:create-folder-dispatcher-and-handler
	       "/images/" (make-pathname
			   :directory '(:absolute "home" "reform" "lisp"
					"site" "reform" "images")))
	      (hunchentoot:create-folder-dispatcher-and-handler
	       "/js/" (make-pathname
		       :directory '(:absolute "home" "reform" "lisp"
				    "site" "reform" "js")))
	      (hunchentoot:create-prefix-dispatcher "/" 'teaser-page)
	      'hunchentoot:default-dispatcher)))

  (push (hunchentoot:create-prefix-dispatcher "/" #'teaser-page)
	hunchentoot:*dispatch-table*)
  (push (hunchentoot:create-folder-dispatcher-and-handler
	 "/css/" (make-pathname
		  :directory '(:absolute "home" "reform" "src"
			       "blueprint-css")))
	hunchentoot:*dispatch-table*)

  (push (hunchentoot:create-folder-dispatcher-and-handler
	 "/images/" (make-pathname
		     :directory '(:absolute "home" "reform" "lisp"
				  "site" "reform" "images")))
	hunchentoot:*dispatch-table*)

  (push (hunchentoot:create-folder-dispatcher-and-handler
	 "/js/" (make-pathname
		 :directory '(:absolute "home" "reform" "lisp"
			      "site" "reform" "js")))
	hunchentoot:*dispatch-table*))


(defparameter *acceptor* (make-instance 'hunchentoot:acceptor :port 4343))

(defun start-reform-site ()
  (get-store)
  (hunchentoot:start *acceptor*))

(defun stop-reform-site ()
  (hunchentoot:stop *acceptor*)
  (ele:close-store *reform-store*)
  (setf *acceptor* nil)
  (setf *reform-store* nil))

(defparameter *css-include* "
  <!-- Framework CSS -->
	<link rel=\"stylesheet\" href=\"/css/blueprint/screen.css\" type=\"text/css\" media=\"screen, projection\">
	<link rel=\"stylesheet\" href=\"/css/blueprint/print.css\" type=\"text/css\" media=\"print\">
        <!--[if IE]>
            <link rel=\"stylesheet\" href=\"/css/blueprint/ie.css\" type=\"text/css\" media=\"screen, projection\">
        <![endif]-->
	<link rel=\"stylesheet\" href=\"/css/blueprint/plugins/fancy-type/screen.css\">
<!-- end Framework CSS -->")

(defparameter *jquery-include* "

")
(defmacro with-standard-page ((&key (title "Welcome to Reform.ie")) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
     (setf (html-mode) :sgml)
     (:html
      (:head (:title (fmt "~A - reform.ie" ,title))
	     (str *css-include*)
	     (:script :type "text/javascript" :src "/js/jquery.js"))
      
      (:body
       ((:div :class "container")
	((:div :class "span-17")
	 (:hr)
	 ((:h1 :class "alt") ((:a :href "/welcome.html") (:img :src "/images/reform.jpg")))
	 ((:h5 :class "alt") (esc "ζον πολιτικόν")))
	((:div :class "span-7 last")
	 ;; (user-pane *standard-output*)
	 )
	;; (:hr)
	;; ((:p :align "center") "News | Articles | Debates")
	(:hr)
	,@body

	(:hr)
	((:h2 :class "alt") (:img :src "/images/reform-footer.jpg"))

	(:hr))))))

(defun teaser-page ()
  (with-standard-page (:title "Welcome to reform.ie")
    (:hr :class "space")
    ((:div :class "span-16 colborder")
     ((:blockquote :style "text-decoration: none;")
      (:p "The Republic guarantees religious and civil
	liberty, equal rights and equal opportunities to all its
	citizens, and declares its resolve to pursue the happiness and
	prosperity of the whole nation and all of its parts,
	cherishing all of the children of the nation equally" ))
     ((:p :align "right") "&mdash;Proclamation of Independence"))))



(hunchentoot:define-easy-handler (first-page :uri "/welcome.html")
    ()
  (with-standard-page (:title "Welcome to the Reform website")
    (str (get-top-tags))
    (:hr)
     (:hr :class "space")
    ((:div :class "span-16 colborder")
     (:blockquote (:p "The Republic guarantees religious and civil
	liberty, equal rights and equal opportunities to all its
	citizens, and declares its resolve to pursue the happiness and
	prosperity of the whole nation and all of its parts,
	cherishing all of the children of the nation equally" ))
     ((:p :align "right") "&mdash;Proclamation of Independence")
     (:hr)
     (str (get-debates)))
    ((:div :class "span-7 last")
     (:h3 "News")
     (str (get-news)))))

(defun get-debates ()
  (with-html-output-to-string (s nil :indent t)
    (when (has-capability* 'poster)
      (htm ((:p :class "edit") ((:a :href "/new.html?type=debate") "Post a new debate"))
	   (:hr :class "space")))
    (let ((top-2 (get-top-n 'debate 2)))
      (htm ((:div :class "span-7 colborder")
	    (display (first top-2) (short-display) s))
	   ((:div :class "span-7 last")
	    (display (second top-2) (short-display) s))))))

(defun get-news ()
  (with-html-output-to-string (s nil :indent t)
    (when (has-capability* 'poster)
      (htm ((:p :class "incr")
	    ((:a :href "/new.html?type=news") "Post a news item"))))
    (dolist (news-item (get-top-n 'news 5))
      (display news-item (short-display) s))))

(defun get-top-tags ()
  (let ((top-3 (get-top-n 'article 3)))
    (with-html-output-to-string (s nil :indent t)
      (when (has-capability* 'poster)
	(htm ((:p :class "incr")
	      ((:a :href "/new.html?type=article") "Post an article"))))
      ((:div :class "span-7 colborder")
       (display (first top-3) (short-display) s))
    
      ((:div :class "span-8 colborder")
       (display (second top-3) (short-display) s))
    
      ((:div :class "span-7 last")
       (display (third top-3) (short-display) s)))))


(defun admin? () t)


(hunchentoot:define-easy-handler (new-object :uri "/new.html")
    ((type :parameter-type #'get-valid-type))
  (if type
      (let ((obj (make-instance type)))
	(with-standard-page (:title "Create new object")
	  ((:div :class "span-16 last")
	      (:h2 (str (string-capitalize (symbol-name type))))
	   (html-form obj *standard-output*))))
      (error "Invalid TYPE: ~S" type)))

(hunchentoot:define-easy-handler (edit-object :uri "/edit.html")
    ((edit :parameter-type 'boolean)
     (display :parameter-type 'boolean)
     (instance-id :parameter-type 'integer)
     (type :parameter-type #'get-valid-type))
  (if-bind
   (obj (ele:get-instance-by-value type 'instance-id instance-id))
   (progn
     (when edit
       (apply-changes obj (regularize-alist (hunchentoot:post-parameters*)))
       (hunchentoot:log-message :info "Applying changes from ~S" (hunchentoot:post-parameters*)))
     (when display
       (hunchentoot:log-message :info "Will now redirect to ~S" (get-redirect-url obj))
       (hunchentoot:redirect (get-redirect-url obj)))
     (with-standard-page (:title "Edit object")
       ((:div :class "span-15 colborder")
	(:h2 (str (string-capitalize type)))
	(when edit (htm (str "object saved")))
	(html-form obj *standard-output*))
       ((:div :class "span-8 last")
	(:h2 "Preview")
	(display obj (short-display) *standard-output*))))
   (error "Object not found")))

  
(defmethod get-redirect-url ((obj reform-base-class))
  (get-url obj))

(defun get-valid-type (type)
  "Check to see whether the symbol from the string TYPE is a valid
  class"
  (when type
    (let ((symbol (read-from-string (string-upcase type))))
      (when (and (find-class symbol nil)
		 (subtypep symbol 'reform-base-class))
	symbol))))

(defun regularize-alist (alist)
  (mapcar (lambda (e)
	    (destructuring-bind (key . value) e
	      (if (stringp key)
		  (cons (read-from-string (string-upcase key))
			value)
		  (cons key value))))
	  alist))

(hunchentoot:define-easy-handler (post-coment :uri "/post-comment.html")
    (in-favour (debate :parameter-type 'integer))
  (when-bind (comment (get-form-field 'comment
				      (regularize-alist
				       (hunchentoot:post-parameters*))))
    (when debate
      (when-bind (obj (ele:get-instance-by-value 'debate 'instance-id debate))
	(let ((comment-obj (make-instance 'comment :comment comment)))
	  (if (equal in-favour "for")
	      (push comment-obj (get-comments-for obj))
	      (push comment-obj (get-comments-against obj)))))))
  (hunchentoot:redirect (hunchentoot:referer)))

(hunchentoot:define-easy-handler (rate :uri "/rate")
    ()
  (hunchentoot:log-message :info "RATE - ~S" (hunchentoot:post-parameters*)))
