(in-package #:reform)

;; threaded comments
;; every comment has a parent and a list containing children
;; threading is done (hopefully) using the blueprint CSS grid

;; print-comment contains a nesting level, starting at zero, and 
;; equating to the number of columns padding on the left
;; comments span N columns, so the total number of comments
;; is number of grid spaces G - N

(defparameter *grid-columns* 24)
(defparameter *comment-span* 16)
(defparameter *max-shrinkage* -10)

(defun get-max-indent ()
  (- *grid-columns* *comment-span* *max-shrinkage*))

(defun get-comment-css (indent)
  (let ((append (- *grid-columns* *comment-span* indent)))
    (cond ((and (> indent 0) (> append 0))
	   (format nil "span-~A prepend-~A append-~A last"
		   *comment-span* indent append))
	  ((and (> indent 0) (<= append 0) (>= append *max-shrinkage*)) 
	   (format nil "span-~A prepend-~A last"
		   (+ *comment-span* append) indent))
	  ((and (> indent 0) (<= append 0)) 
	   (format nil "span-~A prepend-~A last"
		   (+ *comment-span* *max-shrinkage*) (min indent (get-max-indent))))
	  (t (format nil "span-~A append-~A last" *comment-span* append)))))

(defun get-comment-span (indent)
  (if (< indent (- *grid-columns* *comment-span*))
      *comment-span*
      (max (- *grid-columns* indent)  (+ *comment-span* *max-shrinkage*))))

(defmethod print-comment ((c threaded-comment) &optional (indent 0))
  (html ((:div :class (get-comment-css indent))
	 ((:div :class "span-1")
	  (if-bind (user (get-user))
		   (str (cond ((member c (get-liked user)) (print-rating c 'positive))
			      ((member c (get-disliked user)) (print-rating c 'negative))
			      (t (print-rating c 'neutral))))
		   (str (print-rating c 'neutral))))
    
	 ((:div :class (format nil "span-~A last" (1- (get-comment-span indent))))
	  ((:h6 :class "alt")
	   (:b (str (get-author c)) "&ndash;")
	   (str (format-duration (- (get-universal-time) (get-posted c)))) " "
	   (fmt "(Score: ~A)" (get-votes c)))
	  (multiple-value-bind (doc string)
	      (cl-markdown:markdown (get-comment c) :format :html :stream nil)
	    (declare (ignore doc))
	    (str string)))
	 
	  (:p (esc (get-comment c)))
	  (when-bind (user (get-user))
		     (htm ((:p :class "incr")
			   ((:a :class "comment" :href (get-url c)) "link") "&nbsp&nbsp;&nbsp;"
			   ((:a :class "comment" :href (get-url (get-parent c)))
			    "parent") "&nbsp&nbsp;&nbsp;"
			   ((:a :class "comment" :href (format nil "/report-comment?comment=~A" (get-id c)))
			    "report") "&nbsp&nbsp;&nbsp;"
			   ((:a :class "comment" :href "#" :onclick (format nil "javascript:toggle_visible(~A);return false;" (get-id c)))
			    "reply")
			   (if (or (equal (get-author c) (get-username user))
				   (has-capability* 'admin))
			       (htm "&nbsp&nbsp;&nbsp;"
				    ((:a :class "comment" :href (format nil "/delete-comment?comment=~A" (get-id c)))
				     "delete"))))
			  (:p (str (threaded-comment-form c)))))
	 (:hr :class "space"))
	 (if (get-children c)
	     (str (print-comments (get-children c) (1+ indent))))))

(defmethod print-comment ((c deleted-comment) &optional (indent 0))
  (html ((:div :class (get-comment-css indent))
	 "(deleted)")
	(if (get-children c)
	    (str (print-comments (get-children c) (1+ indent))))))


(defun threaded-comment-form (c)
  (html ((:form :id (get-id c) :style "display:none" :method "post" :action "/post-to-thread.html")
	 (:textarea  :style "height: 75px"
		    :name "comment" :rows 5) (:br)
	 ((:button :type "submit" :name "comment") "reply")
	 (:input :type "hidden" :name "type" :value (type-of c))
	 (:input :type "hidden" :name "reply-to" :value (get-id c)))))


(defun flip ()
  (if (zerop (random 2))
      -1 1))

(defun print-comments (comments indent-level)
  (let ((sorted (sort (copy-list comments) #'> :key #'get-votes)))
    (html (dolist (c sorted)
	    (str (print-comment c indent-level))))))

(hunchentoot:define-easy-handler (post-to-thread :uri "/post-to-thread.html")
    (comment type (reply-to :parameter-type 'integer))
  (when-bind (user (get-user))
	     (when (and comment reply-to (get-valid-type type))
	       (let ((parent (ele:get-instance-by-value
			      (get-valid-type type) 'instance-id reply-to)))
		 (add-new-comment parent user comment))))
  (hunchentoot:redirect (hunchentoot:referer)))

(hunchentoot:define-easy-handler (delete-comment :uri "/delete-comment")
    ((comment :parameter-type 'integer))
  (when-bind (user (get-user))
	     (when-bind (obj (ele:get-instance-by-value 'threaded-comment 'instance-id comment))
			(when (or (equal (get-username user) (get-author obj))
				  (has-capability* 'admin))
			  (decf (get-total-comments (get-debate obj)))
			  (change-class obj 'deleted-comment))))
  (hunchentoot:redirect (hunchentoot:referer)))

(hunchentoot:define-easy-handler (report-comment :uri "/report-comment")
    ((comment :parameter-type 'integer))
  (when-bind (user (get-user))
	     (when-bind (obj (ele:get-instance-by-value 'threaded-comment 'instance-id comment))
			(report-to-admins obj (get-user))))
  (hunchentoot:redirect (hunchentoot:referer)))

(defclass abuse-report ()
  ((reported :initarg :reporter :accessor get-reported)
   (comment :initarg :comment :accessor get-comment)
   (when :initarg :when :initform (get-universal-time) :accessor get-when))
  (:metaclass ele:persistent-metaclass))


(defun report-to-admins (comment reporter)
  (dolist (a (get-admins))
    (push (make-instance 'abuse-report :reported reporter
			 :comment comment)
	  (get-messages a))))

(defmethod add-new-comment ((parent article) user comment)
  (push (make-instance 'threaded-comment
		       :parent parent
		       :author (get-username user)
		       :debate parent
		       :comment comment)
	(get-comments parent))
  (incf (get-total-comments parent)))

(defmethod add-new-comment ((parent threaded-comment) user comment)
  (push (make-instance 'threaded-comment
		       :parent parent
		       :debate (get-debate parent)
		       :author (get-username user)
		       :comment comment)
	(get-children parent))
  (incf (get-total-comments (get-debate parent))))

(defparameter *durations*
  '((1 . second) (60 . minute) (3600 . hour ) (86400 . day)
    (204800 . week) (2678400 . month) (31536000 . year)))

(defun find-duration (secs durations &optional saved)
  (cond ((null durations) saved)
	((zerop secs) (car durations))
	((>= secs (caar durations))
	 (find-duration secs (cdr durations) (car durations)))
	(t saved)))

(defun format-duration (seconds &optional (durations *durations*))
  (destructuring-bind (seconds-in-duration . period)
      (find-duration seconds durations)
    (when seconds-in-duration
      (let ((formatstr (format nil "~~A ~A~~:P ago"
			       (string-downcase (symbol-name period)))))
	(format nil formatstr (floor (/ seconds seconds-in-duration)))))))

