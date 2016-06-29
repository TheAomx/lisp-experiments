(ql:quickload :fiasco)

(fiasco:define-test-package :pattern-matcher-test
  (:use :com.theaomx.pattern-matcher))
(in-package :pattern-matcher-test)

(deftest algebra-simplifier-tests ()
  (is (equal (algebraic-simplify 
	      '(+ 1 1))
	     2))
  (is (equal (algebraic-simplify 
	      '(* x 3))
	     '(* 3 x)))
  (is (equal (algebraic-simplify 
	      '(+ 0 x))
	     'x)))

(deftest deriver-tests ()
  (is (equal (derive 
	      (get-derive-expression 1 'x))
	     0))
  (is (equal (derive 
	      (get-derive-expression 'x 'x))
	     1))
  (is (equal (derive
	      (get-derive-expression '(+ x x) 'x))
	     2))
  (is (equal (derive
	      (get-derive-expression '(* x x) 'x))
	     '(+ x x)))
  (is (equal (derive
	      (get-derive-expression '(** x 2) 'x))
	     '(* 2 (** x 1))))
  (is (equal (derive 
	      (get-derive-expression '(com.theaomx.pattern-matcher::ln x) 'x))
	     '(/ 1 x))))

(deftest infix-to-prefix-tests ()
  (is (equal (infix-to-prefix 
	      '(x))
	     'x))
  (is (equal (infix-to-prefix 
	      '(1 + x))
	     '(+ 1 x)))
  (is (equal (infix-to-prefix 
	      '(1 * x))
	     '(* 1 x)))
  (is (equal (infix-to-prefix
	      '(1 + 1 * 1 * 1))
	     '(+ 1 (* 1 (* 1 1)))))
  (is (equal (infix-to-prefix
	      '(1 * 1 + 1 * 1 + 1 * 1))
	     '(+ (* 1 1) (+ (* 1 1) (* 1 1)))))
  (is (equal (infix-to-prefix 
	      '(4 * 3 * 3 + 1))
	     '(+ (* 4 (* 3 3)) 1)))
  (is (equal (infix-to-prefix 
	      '((1 + 1) + (1 * 2)))
	     '(+ (+ 1 1) (* 1 2)))))

(deftest prefix-to-infix-tests ()
  (is (equal (prefix-to-infix 
	      '(1)) 
	     1))
  (is (equal (prefix-to-infix 
	      '(+ 1 2))       
	     '(1 + 2)))
  (is (equal (prefix-to-infix 
	      '(+ (* x x) 2)) 
	     '((x * x) + 2))))

			     
