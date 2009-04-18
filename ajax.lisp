(in-package #:reform)



(ht-ajax:defun-ajax rate-comment (comment vote) (*ajax-processor* :method :post)
;		    (hunchentoot:log-message :info "AJAX - RATE-COMMENT ~A ~A ~A" comment vote)
		    (let* ((user (get-user))
			   (comment-id (parse-integer comment :junk-allowed t))
			   (vote-numeric (parse-integer vote :junk-allowed t))
			   (c (ele:get-instance-by-value 'comment 'instance-id comment-id)))
		      (when (and user c)
			(print-rating c (vote c user vote-numeric)))))



(defun vote (comment user vote)
  (hunchentoot:log-message :info "VOTE comment ~A user ~A vote ~A" comment user vote)
  (if (positive-integer-p vote)
      (cond ((member comment (get-liked user))
	     (setf (get-liked user) (remove comment (get-liked user)))
	     (decf (get-votes comment))
	     'neutral)
	    ((member comment (get-disliked user))
	     (setf (get-disliked user) (remove comment (get-disliked user)))
	     (push comment (get-liked user))
	     (incf (get-votes comment) 2)
	     'positive)
	    (t (push comment (get-liked user))
	       (incf (get-votes comment))
	       'positive))
      (cond ((member comment (get-disliked user))
	     (setf (get-disliked user) (remove comment (get-disliked user)))
	     (decf (get-votes comment))
	     'neutral)
	    ((member comment (get-liked user))
	     (setf (get-liked user) (remove comment (get-liked user)))
	     (push comment (get-disliked user))
	     (decf (get-votes comment) 2)
	     'negative)
	    (t (push comment (get-disliked user))
	       (decf (get-votes comment))
	       'negative))))

(defun print-rating (comment &optional rating)
  (let ((active "cursor: pointer;text-decoration: none;color: red")
	(passive "cursor: pointer;text-decoration: none;")
	(comment-id-string (format nil "comment-~A" (get-id comment))))
    (with-html-output-to-string (s)
      ((:p :id comment-id-string :style "text-align: center;font-size: 16pt;color: #c4b6b6;")
       ((:a :class "rating"
	    :style (case rating (positive active) (otherwise passive))
	    :onclick (format nil "javascript:rate(~A,1)"  (get-id comment))) "+")
       (:br)
       ((:a :class "rating"
	    :style (case rating (negative active) (otherwise passive))
	    :onclick (format nil "javascript:rate(~A,-1)"  (get-id comment))) "-")))))


