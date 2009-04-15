(in-package #:reform)

(defclass display-short ()
  ())

(defclass display-full (display-short)
  ())

(defun short-display ()
  (make-instance 'display-short))

(defun full-display ()
  (make-instance 'display-full))


(defun get-teaser (string &optional (length 140))
  (let* ((teaser-length (min length (length string))))
    (if (< teaser-length length)
	(values string nil)
	(values (subseq string 0 teaser-length) t))))


(defmethod get-url ((obj reform-base-class))
  (format nil "/display.html?type=~A&amp;id=~A"
	  (string-downcase (symbol-name (type-of obj)))
	  (get-id obj)))

(defun get-top-n (class n &key (sort-fn #'>) (key #'get-posted))
  (let ((instances (ele:get-instances-by-class class)))
    (subseq (sort (copy-list instances) sort-fn :key key)
	    0 (min n (length instances)))))

(defmethod display ((obj news) (type display-short)  stream)
  (multiple-value-bind (teaser more?)
      (get-teaser (html->text (get-story obj)))
    (with-html-output (s stream :indent t)
      ((:p :class "incr")
       (:em (str (get-headline obj)))
       " " (str teaser)
       (when more?
	 (htm ((:a :href (get-url obj)) "more...")))))))

(defmethod display ((obj article) (type display-short)  stream)
  (multiple-value-bind (teaser more?)
      (get-teaser (html->text (get-story obj)) 300)
    (with-html-output (s stream :indent t)
      (:h6 ((:a :href (get-url obj)) (str (get-headline obj))) )
      (:p (str teaser)) 
       (when more?
	 (htm ((:a :href (get-url obj)) "more..."))))))

(defmethod display ((obj debate) (type display-short) stream)
  (with-html-output (s stream :indent t)
    ((:h3 :class "alt") ((:a :href (get-url obj)) (str (get-motion obj))) )
    (when-bind (rubric (get-rubric obj))
      (htm (:blockquote (str rubric))))))

(defmethod display :after ((obj debate) (type display-full) stream)
  (with-html-output (s stream :indent t)
    (if (get-user)
	(comment-form obj s)
	(htm ((:a :href "#") "Log in to vote and enter comments")))
    (display-comments obj s)))

(defmethod display-comment ((c comment) (user (eql 'no-user)) stream)
  (with-html-output (s stream :indent t)
    ((:h6 :class "alt")
     (fmt "Posted at ~a by ~A"
	  (timestring (get-posted c))
	  (get-author c)))
    (:p (str (get-comment c)))))

(defmethod display-comment ((c comment) (user user) stream)
  (with-html-output (s stream :indent t)
    ((:div :class "span-1")
     ((:p :style "text-align: center;font-size: 16pt;color: #c4b6b6;")
      ((:a :class "rating" :style "text-decoration: none;" :href "" :id (get-id c)) "+") (:br)
      ((:a :class "rating" :style "text-decoration: none;" :href "" :id (get-id c)) "-")))
    ((:div :class "span-10 last")
     ((:h6 :class "alt")
		 (fmt "Posted at ~a by ~A"
		      (timestring (get-posted c))
		      (get-author c)))
		(:p (esc (get-comment c))))
    (:hr :class "space")))


(defun timestring (universal-time)
  (multiple-value-bind (second minute hour date month year day dl z)
      (decode-universal-time universal-time)
    (declare (ignore second day dl z))
    (format nil "~a:~2,'0d ~a/~a/~a" hour minute date month year)))

(defmethod display-comments ((debate debate) stream)
  (let ((display-context (or (get-user) 'no-user)))
    (with-html-output (s stream :indent t)
      ((:div :class "span-11 colborder")
       (:script :type "text/javascript" :src "/js/rate.js")
       ((:h2 :class "alt") "For")
       (dolist (c (get-comments-for debate))
	 (display-comment c display-context s)))
      ((:div :class "span-12 last")
       ((:h2 :class "alt") "Against")
       (dolist (c (get-comments-against debate))
	 (display-comment c display-context s))))))


(defmethod display ((obj comment) (type display-short) stream)
  (with-html-output (s stream :indent t)
    (multiple-value-bind (second minute hour date month year day dl z)
	(decode-universal-time (get-posted obj))
      (declare (ignore second day dl z))
      (htm ((:h6 :class "alt") (fmt "Posted at ~a:~2,'0d ~a/~a/~a" hour minute date month year))))
    (:p (str (get-comment obj)))))

(defmethod display ((obj news) (type display-full) stream)
  (with-html-output (s stream :indent t)
    (:h2 (str (get-headline obj)))
    (:h5 (str (get-author obj)))
    (str (get-story obj))))

(defmethod get-title ((obj news))
  (get-headline obj))

(defmethod get-title ((obj debate))
  (get-motion obj))

(hunchentoot:define-easy-handler (object-display :uri "/display.html"
						 :default-request-type :get)
    ((id :parameter-type 'integer)
     (type :parameter-type #'get-valid-type))
  (when (and id type)
    (if-bind (obj (ele:get-instance-by-value type 'instance-id id))
	     (with-standard-page (:title (get-title obj))
	       (display obj (full-display) *standard-output*))
	     (error "Object not found"))))

