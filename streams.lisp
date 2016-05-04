
(defconstant the-empty-stream nil)

(defun empty-stream? (s)
    (eq s the-empty-stream))

(defun cons-stream (x y)
  (cons x (delay y)))

(defun delay (x)
  (lambda () x))

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

;(enum-interval 1 5)
;(stream-to-list (filter-stream (lambda (x) (= (rem x 7) 0)) (enum-interval 1 100)))
;(odd-fibs 50)
;(funcall (car (cons (lambda () "hello") (lambda () "lol"))))
