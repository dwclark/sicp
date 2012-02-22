(defpackage :ex1-2 (:use :common-lisp) (:export :translate))
(in-package :ex1-2)
(defun translate ()
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))))

(defpackage :ex1-3 (:use :common-lisp) (:export :sum-squares :sum-squares-cl))
(in-package :ex1-3)

(defun sum-of-squares (x y) 
  (+ (expt x 2) (expt y 2)))

(defun bigger (first second)
  (if (> first second) first
      second))

(defun sum-squares (one two three)
  (sum-of-squares (bigger one two)
		  (bigger two three)))

(defun sum-squares-cl (&rest numbers)
  (let ((sorted (sort numbers #'>)))
    (+ (expt (elt sorted 0) 2) (expt (elt sorted 1) 2))))


(defpackage :ex1-4 (:use :common-lisp) (:export :a-plus-abs-b))
(in-package :ex1-4)
;Because common lisp is a lisp-2 there isn't any
;magic going on here, you have to tell CL exactly
;what to do whereas scheme will figure it out on it's own.
;To be honest, I don't know which I like better at this point,
;CL is less magical, but it may affect the difficulty
;of writing the interpreter in ch 4 & 5
;The CL version is clear, return the correct symbol depending on the conditional,
;then call the function/procedure represented by that symbol with a and b as the arguments
(defun a-plus-abs-b (a b)
  (funcall (if (> b 0) #'+ #'-) a b))

(defpackage :ex1-5 (:use :common-lisp) (:export :p :test))
(in-package :ex1-5)
;CL is applicative, calling (test 0 (p)) will hang because
;CL will evaluate the p before passing it to test which will
;loop forever
(defun p () (p))

(defun test (x y)
  (if (= x 0)
      0
      y))

(defpackage :ex1-6 (:use :common-lisp) (:export :sqrt :average) (:shadow :sqrt))
(in-package :ex1-6)
;In SBCL you get stack exhaustion.  sqrt-iter is called
;over and over again because new if evaluates both the
;then-clause and the else-clause before returning the
;correct value.  This is because new-if is not a special
;form, it is evaluated just like any other function
(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (guess x)
  (< (abs (- (expt guess 2) x)) .001))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
	(t else-clause)))

(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(defun sqrt (x)
  (sqrt-iter 1.0 x))

(defpackage :ex1-7 (:use :common-lisp) (:export :sqrt) 
	    (:import-from :ex1-6 :improve) (:shadow :sqrt))
(in-package :ex1-7)
;The problem is numerical in nature.  For large numbers
;good-enough? may never return true as the computer cannot
;get enough precision in the improve function to ever
;generate numbers that are < .001 (leading to infinite recursion.  
;Smaller numbers have a different
;problem, good-enough? returns true long before the correct
;answer is calculated and return answers that are too big.

;sqrt now works better for smaller and larger numbers
;Smaller numbers work better because the new good-enough? takes into
;account the size of the guess, so for very small numbers the 
;difference will be less than the answer, but before it would be
;greater than the answer and so would return true too early.
;It also works better for larger numbers because the tolerance
;is larger than before, so the function will actually return
;true and the function will terminate, which it did not do before.
(defun good-enough? (guess last-guess)
  (< (abs (/ (- guess last-guess) guess)) .001))

(defun iter (guess last-guess x)
  (if (good-enough? guess last-guess)
      guess
      (iter (improve guess x) guess x)))
    
(defun sqrt (x)
  (iter 1.0 0.0 x))

(defpackage :ex1-8 (:use :common-lisp) (:export :cube-root) 
	    (:import-from :ex1-7 :good-enough?))
(in-package :ex1-8)
;Reuse most of the stuff from exercise 1.7, just need
;to make new improve function
(defun improve (guess x)
  (/ (+ (/ x (expt guess 2)) (* 2 guess)) 3))

(defun iter (guess last-guess x)
  (if (good-enough? guess last-guess)
      guess
      (iter (improve guess x) guess x)))

(defun cube-root (x)
  (iter 1.0 0.0 x))

;Exercise 1.9
;The first one is recursive because the process expands like
;in figure 1.3.  The execution framework must track the data for
;us.  The second is iterative, each successive function call
;has enough information to compute the necessary information,
;the execution framework does not track anything.

(defpackage :ex1-10 (:use :common-lisp) (:export :A :show-values))
(in-package :ex1-10)
;Using wikipedia/wolfram math's definition
(defun A (x y)
  (cond ((= x 0) (+ y 1))
	((= y 0) (A (- x 1) 1))
	(t (A (- x 1) (A x (- y 1))))))

(defun show-values ()
  (format t "(A 1 10): ~A~%" (A 1 10))
  (format t "(A 2 4): ~A~%" (A 2 4))
  (format t "(A 3 3): ~A~%" (A 3 3)))

;(A 0 n) -> computes n + 1
;(A 1 n) -> computes (A 0 (A 1 (- n 1))) -> computes n + 2
;(A 2 n) -> computes 2 + n + n + 1 -> 2n + 3

(defpackage :ex1-11 (:use :common-lisp) (:export :f-iterative :f-recursive))
(in-package :ex1-11)
(defun sum (one two three)
  (+ one (* 2 two) (* 3 three)))

(defun f-recursive (n)
  (cond ((< n 3) n)
	(t (+ (sum (f-recursive (- n 1))
		   (f-recursive (- n 2))
		   (f-recursive (- n 3)))))))

(defun f-iterative (n)
  (labels ((iter (i one two three)
	     (cond ((= n i) (sum one two three))
		   (t (iter (+ i 1) (sum one two three) one two)))))

    (cond ((< n 3) n)
	  ((= n 3) 4)
	  (t (iter 4 4 2 1)))))

(defpackage :ex1-12 (:use :common-lisp) (:export :pascals-tri))
(in-package :ex1-12)
(defun pascals-tri (row col)
  (cond ((or (> col row) (< row 1)) 0)
	((= col 1) 1)
	((= row col) 1)
	(t (+ (pascals-tri (- row 1) (- col 1))
	      (pascals-tri (- row 1) col)))))

;Exercise 1.13 TODO: Make proof
;Exercise 1.14 TODO: Make diagram

(defpackage :ex1-15 (:use :common-lisp) (:export :sine :reset-counter :*counter*))
(in-package :ex1-15)
(defparameter *counter* 0)

(defun reset-counter ()
  (setf *counter* 0))

(defun p (x)
  (incf *counter*)
  (- (* 3 x) (* 4 (* x x x))))

(defun sine (angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;a) Evaluated 5 times when angle -> 12.15
;b) on step n, angle will have been reduced by 3^n, so
;number of evaluations grow on the order of log base 3

(defpackage :ex1-16 (:use :common-lisp) (:export :fast-expt))
(in-package :ex1-16)

(defun fast-expt (b n)
  (labels ((iter (a b n)
	     (cond ((= n 0) a)
		   ((oddp n) (iter (* a b) b (- n 1)))
		   (t (iter a (* b b) (/ n 2))))))
    (iter 1 b n)))

(defpackage :ex1-17 (:use :common-lisp) (:export :fast-mult)
	    (:import-from :ex1-16))
(in-package :ex1-17)
(defun half (x)
  (/ x 2))

(defun double (x)
  (* x 2))

(defun fast-mult (f s)
  (cond ((or (= f 0) (= s 0)) 0)
	((= s 1) f)
	((oddp s) (+ f (fast-mult f (- s 1))))
	(t (fast-mult (double f) (half s)))))

(defpackage :ex1-18 (:use :common-lisp) (:export :fast-mult)
	    (:import-from :ex1-16) (:import-from :ex1-17 :half :double))
(in-package :ex1-18)
;again the key is to pay attention to invariants!
(defun fast-mult (f s)
  (labels ((iter (sum f s)
	     (cond ((or (= f 0) (= s 0)) 0)
		   ((= s 1) (+ sum f))
		   ((oddp s) (iter (+ sum f) f (- s 1)))
		   (t (iter sum (double f) (half s))))))
    (iter 0 f s)))


(defpackage :ex1-19 (:use :common-lisp) (:export :fib))
(in-package :ex1-19)
(defun fib (n)
  (labels ((iter (a b p q count)
	     (cond ((= count 0) b)
		   ((evenp count)
		    (iter a 
			  b 
			  (+ (* p p) (* q q)) ;p'
			  (+ (* 2 q p) (* q q)) ;q'
			  (/ count 2)))
		   (t (iter (+ (* b q) (* a q) (* a p))
			    (+ (* b p) (* a q))
			    p
			    q 
			    (1- count))))))
    
    (iter 1 0 0 1 n)))

;Exercise 1.20
; TODO: Fill in with comment text

(defpackage :ex1-21 (:use :common-lisp) (:export :smallest-divisor :prime?))
(in-package :ex1-21)
;using functions
;199 smallest is 199
;1999 smallest is 1999
;19999 smallest is 7
(defun divides? (a b)
  (= (mod b a) 0))

(defun smallest-divisor (n)
  (labels ((find-divisor (n test-divisor)
	     (cond ((> (* test-divisor test-divisor) n) n)
		   ((divides? test-divisor n) test-divisor)
		   (t (find-divisor n (+ test-divisor 1))))))
    (find-divisor n 2)))

(defun prime? (n)
  (= (smallest-divisor n) n))

;CL makes this a bit easier (time (search-for-primes next num nil))
(defpackage :ex1-22 (:use :common-lisp) (:export :search-for-primes)
	    (:import-from :ex1-21 :prime?))
(in-package :ex1-22)
(defun search-for-primes (next num primes)
  (cond ((= num 0) primes)
	((prime? next) (search-for-primes (+ 2 next) (- num 1) (cons next primes)))
	(t (search-for-primes (+ 2 next) num primes))))
	 
;I had to up the base numbers to get any reasonable numbers
;1) 10000001: 0.001 seconds of real time
;2) 100000001: 0.003 seconds of real time
;3) 1,000,000,001: 0.042 seconds of real time
;The time between 1) and 2) support the (sqrt 10) factor increase in time
;spent doing computations.  2) and 3) do now.  3) took increased by much
;way more than (sqrt 10).  This is because 3) was consing a lot more
;data which is a lot slower than crunching numbers.  So, the run time
;is not proportional to the number of steps, other factors are involved.

(defpackage :ex1-23 (:use :common-lisp) (:export :search-for-primes)
	    (:import-from :ex1-21 :divides?))
(in-package :ex1-23)
(defun next (n)
  (cond ((= n 2) 3)
	(t (+ n 2))))

(defun find-divisor (n test-divisor)
  (labels ((next (num)
	     (cond ((= num 2) 3)
		   (t (+ num 2)))))
    
    (cond ((> (* test-divisor test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (t (find-divisor n (next test-divisor))))))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun prime? (n)
  (= (smallest-divisor n) n))

(defun search-for-primes (next num primes)
  (cond ((= num 0) primes)
	((prime? next) (search-for-primes (+ 2 next) (- num 1) (cons next primes)))
	(t (search-for-primes (+ 2 next) num primes))))

;1) 10000001: 0.001 seconds of real time
;2) 100000001: 0.003 seconds of real time
;3) 1000000001: 0.021 seconds of real time

;In my lisp the first two times are unchanged and the third run is
;exactly half of the third run from the first set of tests.  It also
;consed half as many bytes.  This shows that at least on my machine
;and my lisp implementation what is dominating is the memory allocation
;overhead.

;Exercise 1.26
;It is taking longer now because (expmod base (/ exp 2) m) is being calculated
;twice, with square it is only calculated once.

(defpackage :ex1-27 (:use :common-lisp) (:export :carmichael-test :probably-prime?))
(in-package :ex1-27)
(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((evenp exp)
	 (let ((to-square (expmod base (/ exp 2) m))) 
	   (mod (* to-square to-square) m)))
	(t (mod (* base (expmod base (- exp 1) m)) m))))

(defun try-it (n a)
  (= (expmod a n n) a))

(defun fermat-test (n)
  (try-it n (+ 1 (random (- n 1)))))

(defun probably-prime? (n &optional (times 10))
  (cond ((= times 0) t)
	((fermat-test n) (probably-prime? n (- times 1)))
	(t nil)))

(defun carmichael-test (n)
  (labels ((iter (a)
	     (cond ((= a n) t)
		   ((not (try-it n a)) a)
		   (t (iter (+ a 1))))))
    (iter 2)))

;yep carmichael-test says they are prime when they are not

(defpackage :ex1-29 (:use :common-lisp) (:export :rectangular-integral :simpsons-integral))
(in-package :ex1-29)
(defun sum (term a next b)
  (if (> a b)
      0
      (+ (funcall term a)
	 (sum term (funcall next a) next b))))

(defun rectangular-integral (f a b dx)
  (labels ((next (x) (+ x dx)))
    (* (sum f 
	    (+ a (/ dx 2.0)) 
	    #'next
	    b)
       dx)))

(defun simpson-h (n a b)
  (/ (- b a) n))

(defun simpson-multiplier (n k)
  (cond ((or (= k 0) (= k n)) 1.0)
	((oddp k) 4.0)
	(t 2.0)))
	 
(defun simpson-sum (func n a b k)
  (cond ((> k n) 0)
	(t (+ (* (simpson-multiplier n k) (funcall func (+ a (* k (simpson-h n a b)))))
	      (simpson-sum func n a b (+ k 1))))))

(defun simpson-integral (func n a b)
  (* (/ (simpson-h n a b) 3.0) (simpson-sum func n a b 0)))

(defun simpsons-integral (func n a b)
  (labels ((h () (/ (- b a) n))
	   
	   (multiplier (k)
	     (cond ((or (= k 0) (= k n)) 1.0)
		   ((oddp k) 4.0)
		   (t 2.0)))
	   
	   (sarg (k) (+ a (* k (h))))
	   
	   (term (k) (* (multiplier k) (funcall func (sarg k)))))
    (* (/ (h) 3) (sum #'term 
		      0 
		      #'1+ 
		      n))))
  
;simpson seems to converge faster
;I initially did simpson-integral because nesting functions inside of
;other functions results in SBCL warnings.  It also seemed simpler to me
;I ported simpson-integral-2 from Ken Dyck's blog, using the functions
;I had previously defined from simpson-integral.  Although it took longer
;to do both, I learned a lot more that way.  The big stumblig block I had
;was that a & b in simpson-integral-2 have no relation at all to the 
;a & b in my sum.  I kept making them the same and the code just did not
;work correctly.  Once I stopped being stupid it made a lot of sense.  The
;other trick is in realizing that the term that Simpson needs is related to
;but not the same as the function that is passed in.

(defpackage :ex1-30 (:use :common-lisp) (:export :sum))
(in-package :ex1-30)
(defun sum (term a next b)
  (labels ((iter (a result)
	     (if (> a b) result
		 (iter (funcall next a) (+ result (funcall term a))))))
    (iter a 0)))

(defpackage :ex1-31 (:use :common-lisp) (:export :product :factorial :pi)
	    (:shadow :pi))
(in-package :ex1-31)
;recursive
(defun product-r (func next a b)
  (if (> a b)
      1
      (* (funcall func a) (product-r func next (funcall next a) b))))

;iterative
(defun product (func next a b)
  (labels ((iter (a result)
	     (if (> a b)
		 result
		 (iter (funcall next a) (* result (funcall func a))))))
    (iter a 1)))

(defun factorial (n)
  (product #'identity #'1+ 1 n))

(defun pi (n)
  (labels ((next-frac (a)
	     (let ((tmp (+ a 2)))
	       (/ (* (+ a 1) (+ a 3)) (* tmp tmp))))
	   (next-term (a) (+ a 2)))
    (* 4.0 (product #'next-frac #'next-term 1 n))))

(defpackage :ex1-32 (:use :common-lisp) (:export :accumulate :sum :product))
(in-package :ex1-32)
(defun accumulate-r (combiner null-value func next a b)
  (if (> a b)
      null-value
      (funcall combiner (funcall func a)
	       (accumulate-r combiner null-value func next (funcall next a) b))))

(defun accumulate (combiner null-value func next a b)
  (labels ((iter (a result)
	     (if (> a b)
		 result
		 (iter (funcall next a)
		       (funcall combiner result (funcall func a))))))
    (iter a null-value)))

(defun sum (func a next b)
  (accumulate #'+ 0 func next a b))

(defun product (func next a b)
  (accumulate #'* 1 func next a b))

(defpackage :ex1-33 (:use :common-lisp) 
	    (:export :filtered-accumulate :sum-of-square-of-primes :product-gcd)
	    (:import-from :ex1-21 :prime?))
(in-package :ex1-33)
(defun filtered-accumulate (filter combiner null-value func next a b)
  (cond ((> a b) 
	 null-value)
	((funcall filter a) 
	 (funcall combiner (funcall func a)
		  (filtered-accumulate filter combiner null-value func next (funcall next a) b)))
	(t (filtered-accumulate filter combiner null-value func next (funcall next a) b))))

(defun sum-of-square-of-primes (lower upper)
  (labels ((square (x) (* x x)))
    (filtered-accumulate #'prime? 
			 #'+ 
			 0 
			 #'square 
			 #'1+
			 lower 
			 upper)))

(defun product-gcd (n)
  (labels ((filter (x) (= 1 (gcd n x))))
    (filtered-accumulate #'filter
			 #'* 
			 1 
			 #'identity 
			 #'1+ 
			 2 
			 n)))

;Exercise 1.34
;you get an error, because 2 is not a function
;(defun f (g)
;  (funcall g 2))

(defpackage :fixed-point (:use :common-lisp) (:export :compute :close-enough?))
(in-package :fixed-point)

(defun close-enough? (first second)
  (let ((tolerance 0.00001))
    (< (abs (- first second)) tolerance)))

(defun compute (f first-guess)
  (labels ((try (guess)
	     (let ((next (funcall f guess)))
	       (if (close-enough? guess next)
		   next
		   (try next)))))
    (try first-guess)))

(defpackage :ex1-35 (:use :common-lisp) (:export :phi :phi-damped))
(in-package :ex1-35)
;Divide both sides of the equation on p 38 by phi and you get the fixed point function
(defun phi ()
  (fixed-point:compute (lambda (x) (+ 1 (/ 1 x))) 1.0))

(defun phi-damped ()
  (fixed-point:compute (lambda (x) (/ (+ (+ 1 (/ 1 x)) x) 2)) 1.0))

(defpackage :ex1-36 (:use :common-lisp) (:export :solver :solver-damped)
	    (:import-from :fixed-point :close-enough?))
(in-package :ex1-36)
(defvar *count* 0)

(defun fixed-point-instr (f first-guess)
  (labels ((try (guess)
	     (let ((next (funcall f guess)))
	       (format t "#~d is: ~d~&" (incf *count*) next)
	       (if (close-enough? guess next)
		   next
		   (try next)))))
    (try first-guess)))

;34 runs
(defun solver ()
  (setf *count* 0)
  (fixed-point-instr (lambda (x) (/ (log 1000) (log x))) 2.0))

(defun avg (x y) (/ (+ x y) 2))

;9 runs
(defun solver-damped ()
  (setf *count* 0)
  (fixed-point-instr (lambda (x) (avg (/ (log 1000) (log x)) x)) 2.0))

(defpackage :ex1-37 (:use :common-lisp) (:export :cont-frac :cont-frac-r :phi))
(in-package :ex1-37)
(defun cont-frac-r (n d k)
  (labels ((next (i)
	     (if (= i k) 
		 (/ (funcall n i) (funcall d i))
		 (/ (funcall n i) (+ (funcall d i) (next (+ i 1)))))))
    (next 1)))

(defun cont-frac (n d k)
  (labels ((next (i result)
	     (if (= i 1) 
		 (/ (funcall n 1) result)
		 (next (- i 1) (+ (funcall d (- i 1)) (/ (funcall n i) result))))))
    (next k (funcall d k))))

;1.6180 -> Needs 13 calls to get golden ratio to 4 decimal places
(defun phi (k)
  (let ((f #'(lambda (i) i 1)))
    (/ 1.0 (cont-frac f f k))))

(defpackage :ex1-38 (:use :common-lisp) (:export :euler-e)
	    (:import-from :ex1-37 :cont-frac))
(in-package :ex1-38)
(defun euler-d (n)
  (labels ((is-1 (step)
	     (not (= (mod (+ step 1) 3) 0)))
	   
	   (euler-next (n step total)
	     (cond ((> step n) total)
		   ((is-1 step) (euler-next n (+ step 1) total))
		   (t (euler-next n (+ step 1) (+ total 2))))))

    (if (is-1 n)
	1.0
	(euler-next n 1.0 0))))

;(euler-e 10) -> 2.7182817
(defun euler-e (n)
  (+ 2.0 (cont-frac #'(lambda (i) i 1) #'euler-d n)))

(defpackage :ex1-39 (:use :common-lisp) (:export :tan)
	    (:import-from :ex1-37 :cont-frac) (:shadow :tan))
(in-package :ex1-39)
(defun tan (x k)
  (cont-frac #'(lambda (i)
		 (if (= i 1)
		     x
		     (* -1 (* x x))))
	     #'(lambda (i) (- (* 2.0 i) 1.0))
	     k))

(defpackage :ex1-40 (:use :common-lisp) (:export :newtons-method :cubic :sqrt) (:shadow :sqrt))
(in-package :ex1-40)
(defparameter *dx* 0.001)

(defun deriv (g)
  #'(lambda (x)
      (/ (- (funcall g (+ x *dx*)) (funcall g x)) *dx*)))

(defun newton-transform (g)
  #'(lambda (x)
      (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point:compute (newton-transform g) guess))

(defun sqrt (x)
  (newtons-method #'(lambda (y) (- (* y y) x))
		  1.0))

(defun cubic (a b c)
  (lambda (x)
    (+ (* x x x) (* a (* x x)) (* b x) c)))

(defpackage :ex1-41 (:use :common-lisp) (:export :double-call))
(in-package :ex1-41)
(defun double-call (g)
  (lambda (x)
    (funcall g (funcall g x))))

;(funcall (funcall (double-call (double-call #'double-call)) #'inc) 5) -> 21

(defpackage :ex1-42 (:use :common-lisp) (:export :compose))
(in-package :ex1-42)
(defun compose (f g)
  (lambda (x)
    (funcall f (funcall g x))))

(defpackage :ex1-43 (:use :common-lisp) (:export :repeat-call)
	    (:import-from :ex1-42 :compose))
(in-package :ex1-43)
(defun repeat-call (f n)
  (labels ((do-it (new-func i)
	     (if (= i 0)
		 new-func
		 (do-it (compose f new-func) (- i 1)))))
    (do-it f (- n 1))))

;TODO: Exercise 1.44 & 1.45

(defpackage :ex1-46 (:use :common-lisp) (:export :iterative-improve :sqrt :fixed-point)
	    (:import-from :ex1-6 :average) (:shadow :sqrt))
(in-package :ex1-46)
(defun iterative-improve (good-enough? improve)
  #'(lambda (x)
      (labels ((try (guess)
		 (if (funcall good-enough? guess x)
		     guess
		     (try (funcall improve guess x)))))
	(try 1.0))))


(defun sqrt (x)
  (let ((tolerance 0.001))
    (funcall (iterative-improve #'(lambda (guess x)
				    (< (abs (- (* guess guess) x)) tolerance))
				#'(lambda (guess x)
				    (average guess (/ x guess))))
	     x)))
	   

(defun fixed-point (f)
  (let ((tolerance 0.001))
    (funcall (iterative-improve #'(lambda (guess f)
				    (< (abs (- guess (funcall f guess))) tolerance))
				#'(lambda (guess f)
				    (funcall f guess)))
	     f)))
	   