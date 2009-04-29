(in-package #:reform)

(defparameter *reform-store* nil
  "Holds the elephant store controller object")

(defun get-store ()
  (or *reform-store*
      (setf *reform-store*
	    (elephant:open-store (get-config-option :elephant-spec)))))

(defun get-next-id ()
  (if-bind (current-id (ele:get-from-root "current"))
    (ele:add-to-root "current" (incf current-id)) 
    (ele:add-to-root "current" 0)))

(defclass tag (post)
  ((name :initarg :name :initform nil :accessor get-tag-name :index t)
   (rubric :initarg :rubric :initform nil :accessor get-rubric)
   (objects :initform '() :accessor get-tagged-objects))
  (:metaclass ele:persistent-metaclass))

(defclass reform-base-class ()
  ((instance-id :initarg :id :initform (get-next-id) :accessor get-id :index t)
   (posted :initform (get-universal-time) :accessor get-posted :index t)
   (published :initform nil :accessor published?))
  (:metaclass ele:persistent-metaclass))

(defclass sticky-object-mixin ()
  ((sticky :initarg :sticky :initform nil :accessor sticky?))
  (:metaclass ele:persistent-metaclass))

(defclass tagged-object-mixin ()
  ((tags :initform '() :accessor get-tags))
  (:metaclass ele:persistent-metaclass))

(defclass post (reform-base-class tagged-object-mixin sticky-object-mixin)
  ()
  (:metaclass ele:persistent-metaclass))

(defclass news (post)
  ((headline :initarg :headline :initform "" :accessor get-headline :index t)
   (story :initarg :story :initform "" :accessor get-story)
   (author :initarg :author :initform "" :accessor get-author))
  (:metaclass ele:persistent-metaclass))

(defclass article (news)
  ((content-type :initarg :content-type :initform "text/html" :accessor get-content-type)
   (comments :initarg :comments :initform nil :accessor get-comments)
   (total-comments :initarg :total :initform 0 :accessor get-total-comments))
  (:metaclass ele:persistent-metaclass))

(defclass person (news)
  ()
  (:metaclass ele:persistent-metaclass))

(defclass debate (post)
  ((motion :initarg :motion :initform "" :accessor get-motion)
   (rubric :initarg :rubric :initform nil :accessor get-rubric)
   (comments-for :initform '() :accessor get-comments-for)
   (comments-against :initform '() :accessor get-comments-against))
  (:metaclass ele:persistent-metaclass))

(defclass comment (reform-base-class)
  ((author :initarg :author :initform "Anonymous" :accessor get-author)
   (comment :initarg :comment :initform "" :accessor get-comment)
   (votes :initarg :votes :initform 0 :accessor get-votes)
   (debate :initarg :debate :initform nil :accessor get-debate))
  (:metaclass ele:persistent-metaclass))

(defclass threaded-comment (comment)
  ((parent :initarg :parent :initform nil :accessor get-parent)
   (children :initarg :children :initform nil :accessor get-children))
  (:metaclass ele:persistent-metaclass))

(defclass deleted-comment (threaded-comment)
  ()
  (:metaclass ele:persistent-metaclass))

