;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
(in-package :cl-user)

(defpackage :reform-asd
  (:use :cl :asdf))

(in-package :reform-asd)

(defvar *reform-version* "0.0.1"
  "Current version of the software.")

(export '*reform-version*)

(asdf:defsystem :reform
  :serial t
  :version #.*reform-version*
  :depends-on (:elephant :split-sequence :toc-utils :hunchentoot :alexandria
			 :cl-who :local-time :web-utils :ht-ajax)
  :components ((:file "package")
	       (:file "treat-file")
	       (:file "store")
	       (:file "reform")
	       (:file "ajax")
	       (:file "users")
	       (:file "forms")
	       (:file "display")
	       (:file "tags")
	       (:file "rss")
	       (:file "comments")))
