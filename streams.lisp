
(defconstant the-empty-stream nil)

(defun empty-stream? (s)
    (eq s the-empty-stream))

(defmacro delay (x)
  `(lambda () ,x))

(defmacro cons-stream (x y)
  `(cons ,x (delay ,y)))

(defun force (x)
  (funcall x))

(defun head (s)
  (car s))

(defun tail (s)
  (force (cdr s)))

(defun map-stream (proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream 
       (funcall proc (head s))
       (map-stream proc (tail s)))))

(defun filter-stream (pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((funcall pred (head s))
     (cons-stream (head s)
		  (filter-stream pred (tail s))))
    (T
     (filter-stream pred (tail s)))))

(defun accumulate (combiner init-val s)
  (if (empty-stream? s)
      init-val
      (funcall combiner 
	       (head s)
	       (accumulate combiner 
			   init-val
			   (tail s)))))

(defun append-streams (s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
		   (append-streams (tail s1)
				   s2))))

(defun enum-interval (low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (enum-interval (1+ low) high))))

(defun flatten (stream-of-streams)
  (accumulate #'append-streams
	      the-empty-stream
	      stream-of-streams))

(defun flatmap (f s)
  (flatten (map 'list f s)))

(defun stream-to-list (stream)
  (accumulate #'cons
	      '()
	      stream))

(defun print-stream (stream)
  (when (not (empty-stream? stream))
    (prin1 (head stream))
    (format t " ")
    (print-stream (tail stream))))

(defun nth-stream (stream n)
  (if (= n 0)
      (head stream)
      (nth-stream (tail stream) (- n 1))))

;; example usages of these streams...

(defun fib (n)
  (let ((sum1 1) (sum2 1) (temp 0))
    (dotimes (i (- n 2) sum1)
      (setf temp sum1)
      (setf sum1 (+ sum1 sum2))
      (setf sum2 temp))))
		  
(defun odd-fibs (n)
  (stream-to-list
   (filter-stream
    #'oddp
    (map-stream #'fib (enum-interval 1 n)))))

(defun integers-from (n)
  (cons-stream n (integers-from (1+ n))))

(defun sieve (stream)
  (flet ((divisible? (x y)
	   (= (rem x y) 0)))
    (cons-stream
     (head stream)
     (sieve (filter-stream
	     (lambda (x)
	       (not (divisible? x (head stream))))
	     (tail stream)))))) 

(defun primes ()
  (sieve (integers-from 2)))

(nth-stream (primes)
	    2)

(nth-stream (integers-from 1) 8)
(stream-to-list (filter-stream (lambda (x) (= (rem x 3) 0)) (enum-interval 1 100)))
(odd-fibs 50)
