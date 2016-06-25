(ql:quickload :fiasco)

(in-package #:cl-user)
(defpackage :example-time 
  (:use :common-lisp)
  (:export :seconds 
	   :hours-and-minutes))

(in-package :example-time)

(defun seconds (hours-and-minutes)
  (+ (* 3600 (first hours-and-minutes))
     (* 60 (second hours-and-minutes))))

(defun hours-and-minutes (seconds)
  (list (truncate seconds 3600)
        (/ (rem seconds 3600) 60)))

(fiasco:define-test-package :fiasco-examples
  (:use :example-time))
(in-package :fiasco-examples)

(deftest test-conversion-to-hours-and-minutes ()
  (is (equal (hours-and-minutes 180) '(0 3)))
  (is (equal (hours-and-minutes 4500) '(1 15))))

(deftest test-conversion-to-seconds ()
  (is (= 60 (seconds '(0 1))))
  (is (= 4500 (seconds '(1 15)))))

(deftest double-conversion ()
  (is (= 3600 (seconds (hours-and-minutes 3600))))
  (is (= 1234 (seconds (hours-and-minutes 1234)))))

(deftest failing-testcase ()
  (is (= 1 0)))


