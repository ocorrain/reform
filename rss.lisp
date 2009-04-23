(in-package #:reform)


(hunchentoot:define-easy-handler (rss-feed :uri "/feed")
    ()
  (with-html-output-to-string (s nil :prologue "<?xml version=\"1.0\"?>")
    (:rss :version "2.0" :|xmlns:atom| "http://w3.org/2005/Atom"
          (:channel (:title "reform.ie")
                    (:link "http://reform.ie")
                    (:|atom:link| :href "http://reform.ie/feed" :rel "self" :type "application/rss+xml")
                    (:description "reforming the republic.")
		    (dolist (obj (get-in-posted-order '(article news debate news)))
		      (htm (:item (:title (str (get-title obj)))
				      (:link (str (concatenate 'string "http://reform.ie"
							       (get-url obj)))
				      (:guid  (str (concatenate 'string "http://reform.ie"
								(get-url obj))))
				      ;; (:description (str (html->text (get-rss-description obj))))
				      ))))))))



(defun get-in-posted-order (classes)
  (sort (copy-list (reduce #'append (mapcar #'ele:get-instances-by-class classes)))
	#'> :key #'get-posted))

(defmethod get-rss-description ((obj article))
  (get-teaser (get-story obj)))

(defmethod get-rss-description ((obj debate))
  (get-teaser (get-rubric obj)))

(defmethod get-rss-description ((obj news))
  (get-teaser (get-story obj)))
