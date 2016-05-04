(in-package #:cl-user)
(defpackage :com.theaomx.pattern-matcher
  (:use :common-lisp)
  (:export :simplifier
	   :derive
	   :infix-to-prefix
	   :prefix-to-infix))

(in-package :com.theaomx.pattern-matcher)

(defparameter FAILED 'failed)

(defun atom? (x)
    (and (not (consp x))
         (not (null x))))

(defparameter empty-dict nil)


(defun variable-name (pattern) 
    (cadr pattern))

(defun extend-dict (pat dat dict)
  (let
      ((name (variable-name pat)))
    (let
	((v (assoc name dict)))
      (cond
	((not v) (cons (list name dat) dict))
	((eq (cadr v) dat) dict)
	(T
	 FAILED)))))

(equal (extend-dict '(? x) '10 empty-dict)     '((x 10)))
(equal (extend-dict '(? x) '10 '((x 10)))      '((x 10)))
(equal (extend-dict '(? x) '10 '((x 11)))      FAILED)

(defun lookup (var dict)
    (let 
        ((v (assoc var dict)))
        (if (not v)
            var
            (cadr v))))


(equal (assoc 'x '((x 10)))         '(x 10))
(equal (assoc 'x '((y 10) (x 10)))  '(x 10))
(equal (assoc 'z '((y 10) (x 10)))   nil)


(equal (lookup 'x '((x 10)))       10)
(equal (lookup 'y '((x 10)))       'y)

(defun arbitrary-constant? (pat)
  (and (listp pat) (eq (car pat) '?c)))

(defun constant? (pat)
    (numberp pat))

(defun arbitrary-variable? (pat)
    (and (listp pat) (eq (car pat) '?v)))

(defun variable? (pat)
    (symbolp pat))

(defun arbitrary-expression? (pat)
    (and (listp pat) (eq (car pat) '?)))

(defun match (pat exp dict)
  (cond
    ((and (null pat) (null exp)) dict)
    ((eq dict FAILED) FAILED)
    ((atom? pat)
     (if (atom? exp)
	 (if (eq pat exp)
	     dict
	     FAILED)
	 FAILED))
    ((arbitrary-constant? pat) 
     (if (constant? exp)
	 (extend-dict pat exp dict)
	 FAILED))
    ((arbitrary-variable? pat)
     (if (variable? exp)
	 (extend-dict pat exp dict)      
	 FAILED))
    ((arbitrary-expression? pat)
     (extend-dict pat exp dict))
    ((atom? exp) FAILED)
    (T
     (match
	 (cdr pat) (cdr exp)
	 (match (car pat) (car exp) dict)))))


(equal (match nil nil empty-dict)                         empty-dict)
(equal (match nil nil '((x 10)))                         '((x 10)))
(equal (match '* '* empty-dict)                             empty-dict)

(equal (match '(? x) '10 empty-dict)                       '((x 10)))
(equal (match '(?c x) '10 empty-dict)                      '((x 10)))
(equal (match '(?v x) '10 empty-dict)                       FAILED)
(equal (match '(?v x) 'a empty-dict)                       '((x a)))

(equal (match '((? x) (? x)) '(10 10) empty-dict)          '((x 10)))
(equal (match '((? x) (? x)) '(10 11) empty-dict)           FAILED)
(equal (match '((? x) (? y)) '(10 11) empty-dict)          '((y 11) (x 10)))



(defparameter mult-deriv-pattern '(+ (* (? x) (? y)) (? y)))

(equal (match mult-deriv-pattern '(+ (* 3 x) x) nil)      '((y x) (x 3)))
(equal (match mult-deriv-pattern '(+ (* 3 x) 4) nil)       FAILED)


(defun skeleton-evaluation? (exp)
    (and (consp exp) (eq (car exp) 'q)))

(defun eval-exp (skeleton)
    (cadr skeleton))


(defun evaluate (form dictionary)
    (if (atom? form)
        (lookup form dictionary)

        (apply 
            (lambda (&rest x) (eval (cons (lookup (car form) dictionary) x)))
            (map 'list (lambda (v) (lookup v dictionary))
		 (cdr form)))))

(equal (evaluate '(+ x x) '((x 3))) 6)

(defun in-loop (s dict)
  (cond
    ((null s) nil)
    ((atom? s) s)
    ((skeleton-evaluation? s)
     (evaluate (eval-exp s) dict))
    (T
     (cons (in-loop (car s) dict) (in-loop (cdr s) dict)))))  

(defun instantiate (skeleton dict)
  (in-loop skeleton dict))


(equal (instantiate 'foo empty-dict)           'foo)
(equal (instantiate '(f a b) empty-dict)       '(f a b))

(defparameter dict1 (match mult-deriv-pattern '(+ (* 3 x) x) nil))

(equal (instantiate '(q y) dict1)              'x)
(equal (instantiate '(q x) dict1)              '3)
(equal (instantiate '(+ (q x) (q y)) dict1)    '(+ 3 x))
(equal (instantiate '(q (+ x x)) dict1)        6)


(defun pattern (rule) 
    (car rule))

(defun skeleton (rule) 
    (cadr rule))

(defun simplifier (the-rules)
  (lambda (expression) 
    (labels ((try-rules (exp)
	       (labels 
		   ((scan (rules)
		      (if (null rules)
			  exp
			  (let 
			      ((dictionary 
				(match (pattern (car rules))
				  exp
				  empty-dict)))
			    (if (eq dictionary FAILED)
				(scan (cdr rules))
				(simplify-exp 
				 (instantiate (skeleton (car rules)) dictionary)))))))
		 (scan the-rules)))
	     (simplify-exp (exp)
	       (try-rules 
		(if (consp exp)
		    (map 'list #'simplify-exp exp)
		    exp))))
      (simplify-exp expression))))

(defun get-deriver-rules ()
  '(
    ((dd (?c c) (? v))             0)
    ((dd (?v v) (? v))             1)
    ((dd (?v u) (? v))             0)
    ((dd (+ (? x1) (? x2)) (? v))  (+ (dd (q x1) (q v))
				    (dd (q x2) (q v))))
    ((dd (* (? x1) (? x2)) (? v))  (+ (* (q x1) (dd (q x2) (q v)))
				    (* (dd (q x1) (q v)) (q x2))))
    ((dd (** (? x) (?c n)) (? v))  (* (* (q n) (+ (q x) (q (- n 1))))
				    (dd (q x) (q v))))
    ))

(defun get-algebra-rules ()
  '(
    (((? op) (?c c1) (?c c2))                     (q (op c1 c2)))
    (((? op) (?  e ) (?c c))                      ((q op) (q c) (q e)))
    ((+ 0 (? e))                                  (q e))
    ((* 1 (? e))                                  (q e))
    ((* 0 (? e))                                  0)
    ;((+ (? p1) (? p1))                            (* 2 (q p1)))
    ((* (?c c1) (* (?c c2) (? e )))               (* (q (* c1 c2)) (q e)))
    ((* (?  e1) (* (?c c ) (? e2)))               (* (q c) (* (q e1) (q e2))))
    ((* (* (? e1) (? e2)) (? e3))                 (* (q e1) (* (q e2) (q e3))))
    ((+ (?c c1) (+ (?c c2) (? e )))               (+ (q (+ c1 c2)) (q e)))
    ((+ (?  e1) (+ (?c c ) (? e2)))               (+ (q c ) (+ (q e1) (q e2))))
    ((+ (+ (? e1) (? e2)) (? e3))                 (+ (q e1) (+ (q e2) (q e3))))
    ((+ (* (?c c1) (? e)) (* (?c c2) (? e)))      (* (q (+ c1 c2)) (q e)))
    ((* (? e1) (+ (? e2) (? e3)))                 (+ (* (q e1) (q e2)) (* (q e1) (q e3))))
    ((+ (* (?c c) (? e)) (* (?c c) (? e)))        (* (q (+ c c)) (q e)))
    ))

(defun derive (exp)
  (let ((derive-rules (get-deriver-rules))
	(algebra-rules (get-algebra-rules)))
    (funcall 
     (simplifier 
      algebra-rules) 
     (funcall
      (simplifier 
       derive-rules) 
      exp))))


(com.theaomx.pattern-matcher:derive '(dd (* 2 (* 2 x)) x))
(eval (read-from-string "(derive '(dd (* (* x x) (+ x x)) x))"))

(defun prefix-to-infix (expr)
  (cond ((atom expr)
	 expr)
	((= (length expr) 1)
	 (prefix-to-infix (car expr)))
	(T
	 (list
	  (prefix-to-infix (cadr expr))
	  (car expr)
	  (prefix-to-infix (caddr expr))))))

(equal (prefix-to-infix '(1)) 1)
(equal (prefix-to-infix '(+ 1 2))       '(1 + 2))
(equal (prefix-to-infix '(+ (* x x) 2)) '((x * x) + 2))

(define-condition malformed-infix-error (error)
  ((text :initarg :text :reader text)))

(defun infix-to-prefix (expr)
  (labels ((op-1-found (x)
	     (or (eq x '+) (eq x '-)))
	   (op-2-found (x)
	     (or (eq x '*) (eq x '/)))
	   (split-if-found (pred expr)
	     (let ((found (position-if pred expr)))
	       (if (not (null found))
		   (list (nth found expr)
			 (infix-to-prefix (subseq expr 0 found))
			 (infix-to-prefix (subseq expr (1+ found))))))))
    
    (cond ((atom expr)
	   expr)
	  ((= (length expr) 1)
	   (infix-to-prefix (car expr)))
	  ((member-if #'op-1-found expr)
	   (split-if-found #'op-1-found expr))
	  ((member-if #'op-2-found expr)
	   (split-if-found #'op-2-found expr))
	  (T
	   (error 'malformed-infix-error :text "there was a problem while parsing")))))


(equal (infix-to-prefix '(x))
       'x)
(equal (infix-to-prefix '(1 + x))
       '(+ 1 x))
(equal (infix-to-prefix '(1 * x))
       '(* 1 x))
(equal (infix-to-prefix
	'(1 + 1 * 1 * 1))
       '(+ 1 (* 1 (* 1 1))))

(equal (infix-to-prefix
	'(1 * 1 + 1 * 1 + 1 * 1))
       '(+ (* 1 1) (+ (* 1 1) (* 1 1))))

(equal (infix-to-prefix '(4 * 3 * 3 + 1))
       '(+ (* 4 (* 3 3)) 1))

(infix-to-prefix '((1 + 1) + (1 * 2)))

(defun derive-infix (infix-expr var)
  (derive (list
	   'dd
	   (infix-to-prefix infix-expr)
	   var)))

(infix-to-prefix '(2 * x * x * x))

(infix-to-prefix '(2 * x * x * x))

(funcall (simplifier (get-deriver-rules)) (list 'dd
						(infix-to-prefix '(2 * x * x * x))
						'x))

(derive-infix '(2 * x * x * x) 'x)
