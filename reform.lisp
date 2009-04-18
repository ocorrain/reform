(in-package #:reform)

(defparameter *reform-site* (load-time-value
			     (or #.*compile-file-pathname* *load-pathname*)))


(defparameter *ajax-handler-url* "/ajax")


(defparameter *acceptor* (make-instance 'hunchentoot:acceptor :port 4343))

(defparameter *ajax-processor* (ht-ajax:make-ajax-processor
				:type :lokris
				:server-uri *ajax-handler-url*
				:js-file-uris "/js/lokris.js"
				:acceptor *acceptor*))

(defun get-config-option (option)
  (let ((filespec (make-pathname :defaults *reform-site*
				 :name "my-config"
				 :type "sexp"))
	(orig-filespec (make-pathname :defaults *reform-site*
				 :name "config"
				 :type "sexp")))
    (unless (probe-file filespec)
      (with-simple-restart (accept-default "Create default settings for my-config.sexp and proceed.")
	(error "Missing configuration file: my-config.sexp.  Please copy config.sexp to my-config.sexp and customize for your local environment."))
      (with-open-file (src orig-filespec :direction :input)
	(with-open-file (dest filespec :direction :output)
	  (write (read src) :stream dest))))
    (with-open-file (config filespec)
      (cdr (assoc option (read config))))))


(defun create-handlers ()
  (setq hunchentoot:*dispatch-table*
	(list 'hunchentoot:dispatch-easy-handlers
	      (hunchentoot:create-prefix-dispatcher
	       *ajax-handler-url*
	       (ht-ajax:get-handler *ajax-processor*))
	      (hunchentoot:create-folder-dispatcher-and-handler
	       "/css/" (pathname (get-config-option :css-dir)))
	      (hunchentoot:create-folder-dispatcher-and-handler
	       "/images/" (pathname (get-config-option :image-dir)))
	      (hunchentoot:create-folder-dispatcher-and-handler
	       "/js/" (pathname (get-config-option :js-dir)))
	      (hunchentoot:create-prefix-dispatcher "/" 'teaser-page)
	      'hunchentoot:default-dispatcher)))



(defun start-reform-site ()
  (get-store)
  (create-handlers)
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
(defmacro with-standard-page ((&key ajax (title "Welcome to Reform.ie")) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
     (setf (html-mode) :sgml)
     (:html
      (:head (:title (fmt "~A - reform.ie" ,title))
	     (str *css-include*))
      (:body
       (when (and ,ajax hunchentoot:*session*)
	 (htm (:script :type "text/javascript" :src "/js/rate.js")
	      (str (ht-ajax:generate-prologue *ajax-processor*))))
       
       ((:div :class "container")
	((:div :class "span-17")
	 (:hr)
	 ((:h1 :class "alt") ((:a :href "/welcome.html") (:img :src "/images/reform.jpg")))
	 ((:h5 :class "alt") (esc "ζον πολιτικόν")))
	((:div :class "span-7 last")
	 (user-pane *standard-output*)
	 )
	;; (:hr)
	;; ((:p :align "center") "News | Articles | Debates")
	;; (:hr)
	,@body)))))



(defun teaser-page ()
  (hunchentoot:redirect "/welcome.html")
  ;; (with-standard-page (:title "Welcome to reform.ie")
  ;;   (:hr :class "space")
  ;;   ((:div :class "span-16 colborder")
  ;;    ((:blockquote :style "text-decoration: none;")
  ;;     (:p "The Republic guarantees religious and civil
  ;; 	liberty, equal rights and equal opportunities to all its
  ;; 	citizens, and declares its resolve to pursue the happiness and
  ;; 	prosperity of the whole nation and all of its parts,
  ;; 	cherishing all of the children of the nation equally" ))
  ;;    ((:p :align "right") "&mdash;Proclamation of Independence")))
  )



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
     (when-bind (news (get-news))
		(htm
		 (:h3 "News")
		 (str (get-news)))))))

(defun get-debates ()
  (when-bind (top-2 (get-top-n 'debate 2))
	     (with-html-output-to-string (s nil :indent t)
	       ((:div :class "span-7 colborder")
		       (display (first top-2) (short-display) s))
		      ((:div :class "span-7 last")
		       (display (second top-2) (short-display) s)))))


(defun get-news ()
  (when-bind (top-news (get-top-n 'news 5))
	     (with-html-output-to-string (s nil :indent t)
	       (dolist (news-item top-news)
		 (display news-item (short-display) s)))))


(defun get-top-tags ()
  (let ((top-3 (get-top-n 'article 3)))
    (when top-3
      (with-html-output-to-string (s nil :indent t)
	((:div :class "span-7 colborder")
	 (display (first top-3) (short-display) s))
    
	((:div :class "span-8 colborder")
	 (display (second top-3) (short-display) s))
    
	((:div :class "span-7 last")
	 (display (third top-3) (short-display) s))))))



(hunchentoot:define-easy-handler (new-object :uri "/new.html")
    ((type :parameter-type #'get-valid-type))
  (if type
      (let ((obj (make-instance type)))
	(with-standard-page (:title "Create new object")
	  ((:div :class "span-16 last")
	      (:h2 (str (string-capitalize (symbol-name type))))
	   (html-form obj *standard-output* 'new))))
      (error "Invalid TYPE: ~S" type)))

(hunchentoot:define-easy-handler (delete-object :uri "/delete.html")
    ((instance-id :parameter-type 'integer)
     (type :parameter-type #'get-valid-type))
  (unless (has-capability* 'poster)
    (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+))
  (when-bind (obj (ele:get-instance-by-value type 'instance-id instance-id))
	     (ele:drop-instances (list obj)))
  (hunchentoot:redirect "/"))


(hunchentoot:define-easy-handler (edit-object :uri "/edit.html")
    ((edit :parameter-type 'boolean)
     (display :parameter-type 'boolean)
     (instance-id :parameter-type 'integer)
     (type :parameter-type #'get-valid-type))
  (unless (has-capability* 'poster)
    (hunchentoot:redirect "/"))
  (if-bind
   (obj (ele:get-instance-by-value type 'instance-id instance-id))
   (let ((messages '())
	 (infos '()))
     (when edit
       (multiple-value-bind (o msg inf)
	   (apply-changes obj (regularize-alist (hunchentoot:post-parameters*)))
	 (when msg (setf messages msg))
	 (when inf (setf infos inf)))
       (hunchentoot:log-message :info "Applying changes from ~S" (hunchentoot:post-parameters*)))
     (when display
       (hunchentoot:log-message :info "Will now redirect to ~S" (get-redirect-url obj))
       (hunchentoot:redirect (get-redirect-url obj)))
     (with-standard-page (:title "Edit object")
       ((:div :class "span-15 colborder")
	(:h2 (str (string-capitalize type)))
	(when edit (htm (str "object saved")))
	((:a :href (format nil"/delete.html?instance-id=~A&type=~A"
			   (get-id obj) (symbol-name (type-of obj))))
	 (:img :src "/images/delete.png"))
	(html-form obj *standard-output*))
       ((:div :class "span-8 last")
	(when messages
	  (htm ((:div :class "error")
		(:ul (dolist (m messages)
		       (htm (:li (str m))))))))
	(when infos
	  (htm ((:div :class "success")
		(:ul (dolist (m infos)
		       (htm (:li (str m))))))))
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
    (vote (debate :parameter-type 'integer))
  (when-bind (comment (get-form-field 'comment
				      (regularize-alist
				       (hunchentoot:post-parameters*))))
    (when debate
      (when-bind (obj (ele:get-instance-by-value 'debate 'instance-id debate))
	(let ((comment-obj (make-instance 'comment :comment comment)))
	  (if (equal vote "for")
	      (push comment-obj (get-comments-for obj))
	      (push comment-obj (get-comments-against obj)))))))
  (hunchentoot:redirect (hunchentoot:referer))
  )

(hunchentoot:define-easy-handler (rate :uri "/rate")
    ()
  (hunchentoot:log-message :info "RATE - ~S" (hunchentoot:post-parameters*)))
