;; this lisp program solves the following interesting (programming) task:
;; http://nbviewer.ipython.org/url/norvig.com/ipython/Countdown.ipynb

(defstruct solution-candidate ; example contents of struct fields...
  (numbers)              ; '(1 2 3 4 5 6 7 8 9 10)
  (operators))           ; '(+ + + + - / * + - *)

(defparameter numbers (make-solution-candidate
		       :operators '()
		       :numbers '(1 2 3 4 5 6 7 8 9 10)))
(defparameter ops (list (function +) (function -) (function *) (function /)))

; returns true if solution candidate was fully search (e.g. numbers is a list of 1 element)
(defun evaluated-solution-candidate? (s)
  (= (length (solution-candidate-numbers s)) 1))

(defun eval-operator (operator num1 num2)
  (funcall operator num1 num2))

(defun get-evaluator (solution-candidate)
  (lambda (operator)
    (make-solution-candidate
     :numbers (cons (eval-operator operator
				   (car (solution-candidate-numbers solution-candidate))
				   (cadr (solution-candidate-numbers solution-candidate)))
		    (cddr (solution-candidate-numbers solution-candidate)))
     :operators (cons operator
		      (solution-candidate-operators solution-candidate)))))

(defun apply-all-operators-to-first-two-numbers (solution-candidate)
  (map 'list
       (get-evaluator
	solution-candidate)
       ops))

(defun correct-solution? (candidate)
  (= (car (solution-candidate-numbers candidate)) 2016))

(defun get-results-from-permutations (solution-candidate)
  (declare (optimize speed))
  (cond ((evaluated-solution-candidate? solution-candidate)
	 (if (correct-solution? solution-candidate)
	     solution-candidate))
	(T (map 'list
		#'get-results-from-permutations	   
		(apply-all-operators-to-first-two-numbers solution-candidate)))))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

; example call to get solutions...

(flatten (get-results-from-permutations
 (make-solution-candidate
  :operators '()
  :numbers '(1 2 3 4 5 6 7 8 9 10))))
