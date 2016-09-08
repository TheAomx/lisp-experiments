(in-package #:cl-user)
(defpackage :nodes
  (:use :common-lisp)
  (:export :to-string))

(in-package :nodes)

(defgeneric to-string (node)
  (:documentation "convert a given node to a string"))

(defclass html-attribute ()
  ((name
    :initarg :name
    :accessor name
    :documentation "name of the given html attribute")
   (value
    :initarg :value
    :accessor value
    :documentation "value of the given html attribute")))

(defmethod to-string ((attribute html-attribute))
  (concatenate 'string
	       " " (name attribute)
	       "=\"" (value attribute) "\""))

(defclass text-node ()
  ((value
    :initarg :value
    :accessor value
    :documentation "value of text node")))

(defmethod to-string ((node text-node))
  (value node))

(defclass html-node ()
  ((tag
    :initarg :tag
    :accessor tag
    :documentation "tag of the html node")
   (attributes
    :initarg :attributes
    :initform '()
    :accessor attributes
    :documentation "list of attributes for html node")
   (child-nodes
    :initarg :child-nodes
    :initform '()
    :accessor child-nodes
    :documentation "a list of child nodes")
   (has-ending-tag
    :initarg :has-ending-tag
    :initform T
    :accessor has-ending-tag
    :documentation "set to true of node has ending tag")))

(defmethod to-string ((node html-node))
  (flet ((get-opening-tag ()
	   (flet ((get-attributes ()
		    (reduce (lambda (result attribute)
			      (concatenate 'string
					   result
					   attribute))
			    (map 'list 'to-string (attributes node))
			    :initial-value "")))
	     (concatenate 'string
			  "<"
			  (tag node)
			  (get-attributes)
			  (if (has-ending-tag node)
			      ">"
			      " />"))))

	 (get-child-nodes ()
	   (reduce (lambda (result child-node)
		     (concatenate
		      'string
		      result
		      (to-string child-node)))
		   (child-nodes node)
		   :initial-value ""))
	   
	 (get-ending-tag ()
	   (concatenate 'string "</" (tag node) ">")))

    
    (if (has-ending-tag node)
	(concatenate 'string
		     (get-opening-tag) 
		     (get-child-nodes)
		     (get-ending-tag))
	(concatenate 'string
		     (get-opening-tag)))))



(let* ((text-node (make-instance 'text-node :value "hello world"))
       (attribute (make-instance 'html-attribute :name "href" :value "google.de"))
       (p (make-instance 'html-node
			 :child-nodes (list text-node)
			 :attributes '()
			 :tag "p"))
       (html-node (make-instance 'html-node
				 :child-nodes (list p)
				 :attributes (list attribute)
				 :tag "a")))
  (to-string html-node))


(flet ((h1 (content) (make-instance 'html-node
				    :child-nodes (list (make-instance 'text-node
								      :value content))
				    :attributes '()
				    :tag "h1")))
  (to-string (h1 "hello world")))
