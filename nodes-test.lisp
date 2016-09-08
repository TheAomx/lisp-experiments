(ql:quickload :fiasco)

(fiasco:define-test-package :nodes-test
  (:use :nodes))
(in-package :nodes-test)

(deftest simple-text-evaluates-to-itself ()
  (let* ((text-node (make-instance 'nodes::text-node :value "hello world")))
    (is (string= (to-string text-node) 
	       "hello world"))))

(deftest simple-html-node ()
  (let* ((html-node (make-instance 'nodes::html-node
				   :tag "p")))
    (is (string= (to-string html-node)
	       "<p></p>"))))

(deftest simple-html-node-without-ending-tag ()
  (let* ((html-node (make-instance 'nodes::html-node
				   :tag "br"
				   :has-ending-tag nil)))
    (is (string= (to-string html-node)
	       "<br />"))))


(deftest simple-html-node-with-text-node ()
  (let* ((text-node (make-instance 'nodes::text-node
				   :value "hello world"))
	 (html-node (make-instance 'nodes::html-node
				   :tag "p"
				   :child-nodes (list text-node))))
    (is (string= (to-string html-node)
	       "<p>hello world</p>"))))

(deftest simple-html-node-with-attribute ()
  (let* ((attribute (make-instance 'nodes::html-attribute
				   :name "href"
				   :value "#asd"))
	 (html-node (make-instance 'nodes::html-node
				   :tag "a"
				   :attributes (list attribute))))
    (is (string= (to-string html-node)
	       "<a href=\"#asd\"></a>"))))
  

(deftest simple-html-node-with-attribute-and-text-node ()
  (let* ((attribute (make-instance 'nodes::html-attribute
				   :name "href"
				   :value "#asd"))
	 (text-node (make-instance 'nodes::text-node
				   :value "hello world"))
	 (html-node (make-instance 'nodes::html-node
				   :tag "a"
				   :child-nodes `(,text-node)
				   :attributes (list attribute))))
    (is (string= (to-string html-node)
	       "<a href=\"#asd\">hello world</a>"))))

(deftest nested-html-node ()
  (let* ((html-node-1 (make-instance 'nodes::html-node
				     :tag "p"))
	 (html-node-2 (make-instance 'nodes::html-node
				     :tag "span"
				     :child-nodes `(,html-node-1))))
    (is (string= (to-string html-node-2)
	       "<span><p></p></span>"))))


(deftest nested-html-node-with-text-node ()
  (let* ((text-node (make-instance 'nodes::text-node
				   :value "hello world"))
	 (p (make-instance 'nodes::html-node
				     :tag "p"
				     :child-nodes `(,text-node)))
	 (span (make-instance 'nodes::html-node
				     :tag "span"
				     :child-nodes `(,p))))
    (is (string= (to-string span)
		 "<span><p>hello world</p></span>"))))


(deftest nested-html-node-with-text-and-attribute-node ()
  (let* ((text-node (make-instance 'nodes::text-node
				   :value "hello world"))
	 (attribute (make-instance 'nodes::html-attribute
				   :name "class"
				   :value "test"))
	 (p (make-instance 'nodes::html-node
				     :tag "p"
				     :attributes (list attribute)
				     :child-nodes `(,text-node)))
	 (span (make-instance 'nodes::html-node
				     :tag "span"
				     :attributes (list attribute)
				     :child-nodes `(,p))))
    (is (string= (to-string span)
		 "<span class=\"test\"><p class=\"test\">hello world</p></span>"))))


