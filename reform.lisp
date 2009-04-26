(in-package #:reform)

(defmacro html (&body body)
  (let ((g (gensym "html-output")))
    `(with-html-output-to-string (,g)
       ,@body)))

(defparameter *reform-site* (load-time-value
			     (or #.*compile-file-pathname* *load-pathname*)))


(defparameter *ajax-handler-url* "/ajax")

(defparameter *nuggets* '("if not now, when?"
			  "reforming the republic"
			  "business as usual?"))

(defun get-nugget ()
  (elt *nuggets* (random (length *nuggets*))))

(defvar *acceptor* (make-instance 'hunchentoot:acceptor :port 4343))

(defvar *ajax-processor* (ht-ajax:make-ajax-processor
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
	      (hunchentoot:create-static-file-dispatcher-and-handler
	       "/css/reform.css" (merge-pathnames (pathname "css/reform.css")
						  (make-pathname :directory (pathname-directory *reform-site*))))
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
	<link rel=\"stylesheet\" href=\"/css/blueprint/plugins/fancy-type/screen.css\" type=\"text/css\">
<!-- end Framework CSS -->
	<link rel=\"stylesheet\" href=\"/css/reform.css\" type=\"text/css\">
")

(defparameter *twitter-flash-include*
  "
<object classid=\"clsid:d27cdb6e-ae6d-11cf-96b8-444553540000\"
codebase=\"http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=9,0,124,0\"
width=\"290\" height=\"350\" id=\"TwitterWidget\" align=\"middle\">
       <param name=\"allowScriptAccess\" value=\"always\" />
       <param name=\"allowFullScreen\" value=\"false\" />
       <param name=\"movie\"
value=\"http://static.twitter.com/flash/widgets/profile/TwitterWidget.swf\"
/>
       <param name=\"quality\" value=\"high\" />
       <param name=\"bgcolor\" value=\"#000000\" />
       <param name=\"FlashVars\"
value=\"userID=32971071&styleURL=http://static.twitter.com/flash/widgets/profile/velvetica.xml\">
       <embed src=\"http://static.twitter.com/flash/widgets/profile/TwitterWidget.swf\"
quality=\"high\" bgcolor=\"#000000\" width=\"290\" height=\"350\"
name=\"TwitterWidget\" align=\"middle\" allowScriptAccess=\"sameDomain\"
allowFullScreen=\"false\" type=\"application/x-shockwave-flash\"
pluginspage=\"http://www.macromedia.com/go/getflashplayer\"
FlashVars=\"userID=32971071&styleURL=http://static.twitter.com/flash/widgets/profile/velvetica.xml\"/>
</object>
")

(defparameter *twitter-include* "

<div class=\"info\" id=\"twitter_div\">
<h3 class=\"alt\">reformdotie@twitter</h2>
<a href=\"http://twitter.com/reformdotie\" id=\"twitter-link\"
style=\"display:block;\">follow us</a>
</div>
<ul id=\"twitter_update_list\"></ul>

<script type=\"text/javascript\"
src=\"http://twitter.com/javascripts/blogger.js\"></script>
<script type=\"text/javascript\"
src=\"http://twitter.com/statuses/user_timeline/reformdotie.json?callback=twitterCallback2&amp;count=5\"></script>
")

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
       (when (and ,ajax (boundp 'hunchentoot:*session*) hunchentoot:*session*)
	 (htm (:script :type "text/javascript" :src "/js/rate.js")
	      (str (ht-ajax:generate-prologue *ajax-processor*))))
       
       ((:div :class "container")
	((:div :class "span-15")
	 (:hr)
	 ((:h1 :class "alt") ((:a :href "/welcome.html") (:img :src "/images/reform.jpg" :alt "reform.ie")))
	 ((:h3 :class "alt") (str (get-nugget))))
	((:div :class "span-9 last")
	 (user-pane *standard-output*))
	;; ((:div :class "span-24 last")
	;;  (str (print-menu)))
	,@body)))))
;(str (print-menu))
(defun print-menu ()
  (with-html-output-to-string (s)
    ((:p :class "menulink")
     ((:a :class "menulink" :href "/policy.html") "policy") "  /  "
     ((:a :class "menulink" :href "/articles.html") "articles") "  /  "
     ;; ((:a :class "menulink" :href "/news.html") "news") "  /  "
     ;; ((:a :class "menulink" :href "/debates.html") "debates") "  /  "
     ((:a :class "menulink" :href "/login.html") "login or register") "  /  "
     ((:a :class "menulink" :href "/about.html") "about us"))))




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
     ((:blockquote :style "font-size:14pt;font-family:sans-serif;font-style:normal;")
      (:p "The Republic guarantees religious and civil
	liberty, equal rights and equal opportunities to all its
	citizens, and declares its resolve to pursue the happiness and
	prosperity of the whole nation and all of its parts,
	cherishing all of the children of the nation equally" ))
     ((:p :align "right") "&mdash;Proclamation of Irish independence, Easter Monday 1916")
     (:hr)
     (str (get-debates)))
    ((:div :class "span-7 last")
     (when-bind (news (get-news))
     		(htm
     		 (:h3 "News")
     		 (str news)))
     (str *twitter-include*)
     ;(str *twitter-flash-include*)
 
     )))

(defun get-debates ()
  (when-bind (top-2 (get-top-n 'debate 2))
	     (with-html-output-to-string (s nil :indent t)
	       ((:div :class "span-15 last")
		((:h2 :class "alt") "Debates"))
	       ((:div :class "span-7 colborder")
		(str (display (first top-2) (short-display))))
	       ((:div :class "span-7 last")
		(str (display (second top-2) (short-display)))))))


(defun get-news ()
  (when-bind (top-news (get-top-n 'news 5))
	     (with-html-output-to-string (s nil :indent t)
	       (dolist (news-item top-news)
		 (display news-item (short-display))))))


(defun get-top-tags ()
  (let ((top-2 (get-top-with-sticky 'tag 2)))
    (when top-2
      (with-html-output-to-string (s)
	((:div :class "span-11 colborder")
	 (htm (str (display (get-tag-by-name "Local government") (short-display)))))
	((:div :class "span-12 last")
	 (htm (str (display (get-tag-by-name "European Union") (short-display)))))))))

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
     (with-standard-page (:title "Edit object" :ajax t)
       ((:div :class "span-15 colborder")
	(:h2 (str (string-capitalize type)))
	(when edit (htm (str "object saved")))
	((:a :href (format nil"/delete.html?instance-id=~A&type=~A"
			   (get-id obj) (symbol-name (type-of obj))))
	 (:img :src "/images/delete.png"))
	(html-form obj *standard-output*)
	(when (typep obj 'tagged-object-mixin)
	  (htm ((:hr) ((:p :id "tag-cloud")
		       (print-tags-for-editing obj))))))
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
	(str (display obj (short-display))))))
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
	(let ((comment-obj (make-instance 'comment :comment comment :author (get-username (get-user)))))
	  (if (equal vote "for")
	      (push comment-obj (get-comments-for obj))
	      (push comment-obj (get-comments-against obj)))))))
  (hunchentoot:redirect (hunchentoot:referer)))


(hunchentoot:define-easy-handler (rate :uri "/rate")
    ()
  (hunchentoot:log-message :info "RATE - ~S" (hunchentoot:post-parameters*)))




(defun class-page (class-name title)
  (balanced-list-page title (ele:get-instances-by-class class-name)))

(defun balanced-list-page (title instances)
  (let ((midpoint (ceiling (/ (length instances) 2))))
    (with-standard-page (:title title)
      ((:div :class "span-24 last")
       ((:h1 :class "alt") (str title)))
      (if instances
	  (htm ((:div :class "span-11 colborder")
		(dolist (a (subseq instances 0 midpoint))
		  (htm (str (display a (short-display)))
		       (:hr))))
	       ((:div :class "span-12 last")
		(dolist (a (subseq instances midpoint))
		  (htm (str (display a (short-display)))
		       (:hr)))))
	  (htm ((:div :class "span-24 last") "No news"))))))



(defun get-in-tag-order (type)
  (remove-duplicates
   (reduce #'append (mapcar (lambda (tag)
			      (get-tagged-of-type tag type))
			    (get-top-with-sticky 'tag most-positive-fixnum)))
   :key #'get-id :from-end t))

(hunchentoot:define-easy-handler (articles-page :uri "/articles.html")
    ()
  (balanced-list-page "Articles" (get-in-tag-order 'article)))

(hunchentoot:define-easy-handler (debates-page :uri "/debates.html")
    ()
  (class-page 'debate "Debates"))

(hunchentoot:define-easy-handler (news-page :uri "/news.html")
    ()
  (class-page 'news "News"))

(hunchentoot:define-easy-handler (policy-areas :uri "/policy.html")
    ()
  (class-page 'tag "Policy areas"))

(hunchentoot:define-easy-handler (about-page :uri "/about.html")
    ()
  (let* ((all-instances (ele:get-instances-by-class 'person))
	 (midpoint (ceiling (/ (length all-instances) 2))))
    (with-standard-page (:title "about reform.ie")
      ((:div :class "span-16 prepend-4 append-4")
       ((:h1 :class "alt") (str "About reform.ie"))
       ((:blockquote :style "font-size:12pt;font-family:sans-serif;color:black;font-style: normal;text-align:justify;") 
	"reform.ie is an internet-based political platform that
       advocates the reform of local government in Ireland and the
       continuing reform of the European Union with Ireland at its
       core." (:br)
	"We believe that these objectives are shared by a large
proportion of the Irish electorate and we aim to provide a forum for debate to
register this. " (:br)

	"We hope to secure the commitment of candidates, both local and
European, to propose &amp; deliver the reforms so urgently
required if elected." (:br) 

"reform.ie is not linked to any existing political party or movement
in Ireland or elsewhere, nor are we funded by any political or
economic entity." 
 )
       
       ((:h1 :class "alt") "Contact details")
       ((:blockquote :style "font-size:12pt;font-family:sans-serif;color:black;font-style: normal;text-align:justify;")
	(str "General contact &mdash; ") ((:a :href "mailto:info@reform.ie") (:i "info@reform.ie"))
	(:br) (:br)
	(dolist (p (ele:get-instances-by-class 'person))
	  (htm (str (display p (short-display)))
	       (:br) (:br))))))))

