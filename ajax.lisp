(in-package #:reform)

(defparameter *ajax-handler-url* "/ajax")

(defparameter *ajax-processor* (ht-ajax:make-ajax-processor
				:type :lokris
				:server-uri *ajax-handler-url*
				:js-file-uris "/js/lokris.js"))

(push (hunchentoot:create-prefix-dispatcher *ajax-handler-url*
					    (ht-ajax:get-handler *ajax-processor*))
      hunchentoot:*dispatch-table*)

(ht-ajax:defun-ajax rate-comment (comment vote) (*ajax-processor* :method :post)
		    (let ((user (get-user))
			  (c (ele:get-instance-by-value 'comment 'instance-id comment)))
		      (when (and user c)
			(print-rating (vote c user vote)))))

(defun vote (comment user vote)
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

(defun print-rating (rating)
  (case rating
    (positive )
    (negative )
    (neutral  )))

