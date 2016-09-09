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

(defun get-evaluator (num1 num2)
  (lambda (operator)
    (cons operator (eval-operator operator num1 num2))))

(defun make-solution-candidates-from-partial-results (nums operators partial-results)
  (map 'list
       (lambda (result)
	 (make-solution-candidate :numbers (cons (cdr result)
						 (cddr nums))
				  :operators (cons (car result)
						   operators)))
       partial-results))

(defun apply-all-operators-to-first-two-numbers (nums)
  (map 'list
       (get-evaluator
	(car nums)
	(cadr nums))
       ops))

(defun correct-solution? (result)
  (= result 2016))

(defun get-results-from-permutations (solution-candidate)
  (let ((nums (solution-candidate-numbers solution-candidate))
	(operators (solution-candidate-operators solution-candidate)))
    (cond ((evaluated-solution-candidate? solution-candidate)
	   (if (correct-solution? (car nums))
	       solution-candidate))
	  (T (map 'list
		  #'get-results-from-permutations	   
		  (make-solution-candidates-from-partial-results nums
								 operators
								 (apply-all-operators-to-first-two-numbers nums)))))))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

; example call to get solutions...

(flatten (get-results-from-permutations
 (make-solution-candidate
  :operators '()
  :numbers '(1 2 3 4 5 6 7 8 9 10))))
