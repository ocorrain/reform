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

(defun get-top-with-sticky (class n)
  (let ((instances (ele:get-instances-by-class class)))
    (subseq (sort (copy-list instances)
		  (lambda (i1 i2)
		    (cond ((and (sticky? i1)
				(sticky? i2))
			   (< (get-posted i1) (get-posted i2)))
			  ((sticky? i1) t)
			  ((sticky? i2) nil)
			  (t (< (get-posted i1) (get-posted i2))))))
	    0 (min n (length instances)))))

(defmethod display :before ((obj post) (type display-short) stream)
  (when (has-capability* 'poster)
    (with-html-output (s stream)
      ((:a :href (format nil "/edit.html?instance-id=~A&type=~A"
			 (get-id obj) (symbol-name (type-of obj))))
       "[ edit ]"))))


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
      ((:h3 :class "alt") "Article: " ((:a :href (get-url obj)) (str (get-headline obj))) )
      (:p (str teaser)
	  (when more?
	 (htm ((:a :href (get-url obj)) "(more)")))))))


(defmethod display ((obj article) (type display-full) stream)
  (with-html-output (s stream :indent t)
    (:h1 (str (get-title obj)))
    (:p (:i (fmt "&mdash;by ~A" (get-author obj))))
    (:p (print-tag-links obj s))
    (str (get-story obj))))

(defmethod display ((obj user) (type display-short) stream)
  (with-html-output (s stream)
    (:h6 (str (get-username obj)))
    (:ul (dolist (c (get-capabilities obj))
	   (htm (:li (str c)))))))

(defmethod display ((obj debate) (type display-short) stream)
  (with-html-output (s stream :indent t)
    ((:h3 :class "alt") "Debate: " ((:a :href (get-url obj)) (str (get-motion obj))) )
    (when-bind (rubric (get-rubric obj))
      (htm (:blockquote (str rubric))))))

(defmethod display :after ((obj debate) (type display-full) stream)
  (with-html-output (s stream :indent t)
    (print-tag-links obj s)
    (if (get-user)
	(comment-form obj s)
	(htm ((:a :href "/login.html") "Log in to vote and enter comments")))
    (display-comments obj s)))

(defmethod display ((obj tag) (type display-short) stream)
  (with-html-output (s stream :indent t)
    ((:h3 :class "alt") ((:a :href (get-url obj)) (str (get-tag-name obj))) )
    (when-bind (rubric (get-rubric obj))
      (htm (:blockquote (str rubric))))))

(defmethod display ((obj tag) (type display-full) stream)
  (with-html-output (s stream :indent t)
    (:h1 (str (get-tag-name obj)) )
    (when-bind (rubric (get-rubric obj))
	       (htm (:blockquote (str rubric))))
    (let* ((objects (get-tagged-objects obj))
	   (midpoint (ceiling (/ (length objects) 2))))
      (when objects
	(htm ((:div :class "span-11 colborder")
	      (dolist (o (subseq objects 0 midpoint))
		(display o (short-display) s)
		(htm (:hr))))
	     ((:div :class "span-12 last")
	      (dolist (o (subseq objects midpoint))
		(display o (short-display) s)
		(htm (:hr)))))))))



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
     (str (cond ((member c (get-liked user)) (print-rating c 'positive))
		((member c (get-disliked user)) (print-rating c 'negative))
		(t (print-rating c 'neutral)))))
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

(defun sort-comments (comments)
  (sort (copy-list comments) #'> :key #'get-votes))

(defun clean-comments (debate)
  (let ((for (get-comments-for debate))
	(against (get-comments-against debate)))
    (setf (get-comments-for debate) (remove nil for)
	  (get-comments-against debate) (remove nil against))))

(defmethod display-comments ((debate debate) stream)
  (clean-comments debate)
  (let ((display-context (or (get-user) 'no-user)))
    (with-html-output (s stream :indent t)
      ((:div :class "span-11 colborder")
       (:script :type "text/javascript" :src "/js/rate.js")
       ((:h2 :class "alt") "For")
       (dolist (c (sort-comments (remove nil (get-comments-for debate))))
	 (display-comment c display-context s)))
      ((:div :class "span-12 last")
       ((:h2 :class "alt") "Against")
       (dolist (c (sort-comments (remove nil (get-comments-against debate))))
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
    (print-tag-links obj s)
    (str (get-story obj))))

(defmethod get-title ((obj news))
  (get-headline obj))

(defmethod get-title ((obj debate))
  (get-motion obj))

(defmethod get-title ((obj tag))
  (get-tag-name obj))

(hunchentoot:define-easy-handler (object-display :uri "/display.html"
						 :default-request-type :get)
    ((id :parameter-type 'integer)
     (type :parameter-type #'get-valid-type))
  (when (and id type)
    (if-bind (obj (ele:get-instance-by-value type 'instance-id id))
	     (with-standard-page (:title (get-title obj) :ajax t)
	       ((:div :class "span-24 last")
		(display obj (full-display) *standard-output*)))
	     (error "Object not found"))))

