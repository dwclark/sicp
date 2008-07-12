;Exercise 1.2
(defun translate-expression ()
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))))

;Exercise 1.3
(defun square (x)
  (* x x))

(defun sum-of-squares (x y)
  (+ (square x) (square y)))
 
(defun bigger (first second)
  (cond ((> first second) first)
	(t second)))1

;when you get the trick it sure is easier
(defun sum-squares-largest (one two three)
  (sum-of-squares (bigger one two)
		  (bigger two three)))

;Exercise 1.4
;Because common lisp is a lisp-2 there isn't any
;magic going on here, you have to tell CL exactly
;what to do whereas scheme will figure it out on it's own.
;To be honest, I don't know which I like better at this point,
;CL is less magical, but it may affect the difficulty
;of writing the interpreter in ch 4 & 5
(defun a-plus-abs-b (a b)
  (funcall (if (> b 0) #'+ #'-) a b))

;Exercise 1.5
;CL is applicative, calling (test 0 (p)) will hang because
;CL will evaluate the p before passing it to test which will
;loop forever
(defun p () (p))

(defun test (x y)
  (if (= x 0)
      0
      y))

;Exercise 1.6
;In SBCL you get stack exhaustion.  sqrt-iter is called
;over and over again because new if evaluates both the
;then-clause and the else-clause before returning the
;correct value.  This is because new-if is not a special
;form, it is evaluated just like any other function
(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) .001))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
	(t else-clause)))

(defun sqrt-iter (guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(defun my-sqrt (x)
  (sqrt-iter 1.0 x))

;Exercise 1.7
;The problem is numerical in nature.  For large numbers
;good-enough? may never return true as the computer cannot
;get enough precision in the improve function to ever
;generate numbers that are < .001 (leading to infinite recursion.  
;Smaller numbers have a different
;problem, good-enough? returns true long before the correct
;answer is calculated and return answers that are too big.

(defun good-enough-2? (guess last-guess)
  (< (abs (/ (- guess last-guess) guess)) .001))

(defun sqrt-iter-2 (guess last-guess x)
  (if (good-enough-2? guess last-guess)
      guess
      (sqrt-iter-2 (improve guess x) guess x)))
    
(defun my-sqrt-2 (x)
  (sqrt-iter-2 1.0 0.0 x))

;my-sqrt-2 works better for smaller and larger numbers
;Smaller numbers work better because the good-enough-2? takes into
;account the size of the guess, so for very small numbers the 
;difference will be less than the answer, but before it would be
;greater than the answer and so would return true too early.
;It also works better for larger numbers because the tolerance
;is larger than before, so the function will actually return
;true and the function will terminate, which it did not do before.

;Exercise 1.8
;Reuse most of the stuff from exercise 1.7, just need
;to make new improve function
(defun cube-root-improve (guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(defun cube-root-iter (guess last-guess x)
  (if (good-enough-2? guess last-guess)
      guess
      (cube-root-iter (cube-root-improve guess x) guess x)))

(defun cube-root (x)
  (cube-root-iter 1.0 0.0 x))

(defun cube (x)
  (* x x x))

;Exercise 1.9
;The first one is recursive because the process expands like
;in figure 1.3.  The execution framework must track the data for
;us.  The second is iterative, each successive function call
;has enough information to compute the necessary information,
;the execution framework does not track anything.

;Exercise 1.10
;I believe SICP is wrong here.  I looked up the definition 
;for the Ackerman function on both Wikipedia and Wolfram.
;Both give the same definition, which is below, and it is not
;the definition in SICP.  I don't really know why this is, but
;the definitions are clearly not the same.
;See Wikipedia for a table of functions computed for an
;instantiation of an Ackerman function for given m's
(defun Wiki-A (m n)
  (cond ((= m 0) (+ n 1))
	((= n 0) (Ackerman (- n 1) 1))
	(t (Ackerman (- m 1) (Ackerman m (- n 1))))))

;Exercise 1.11
;helper function to prevent boredom and typing mistakes
(defun sum-1.11 (one two three)
  (+ one (* 2 two) (* 3 three)))

(defun ex-1.11-r (n)
  (cond ((< n 3) n)
	(t (+ (sum-1.11 (ex-1.11-r (- n 1))
			(ex-1.11-r (- n 2))
			(ex-1.11-r (- n 3)))))))

;need to do this by hand a few times to see where it is going
;then you over complicate things, only to find out
;it's much simpler than you imagined
(defun ex-1.11-iter (n i one two three)
  (cond ((= n i) (sum-1.11 one two three))
	(t (ex-1.11-iter n (+ i 1) (sum-1.11 one two three) one two))))

;I found it simpler to just pre-calculate n up to three, it makes
;ex-1.11-iter much simpler
(defun ex-1.11-i (n)
  (cond ((< n 3) n)
	((= n 3) 4)
	(t (ex-1.11-iter n 4 4 2 1))))
			 
		      
;Exercise 1.12
(defun pascals-tri (row col)
  (cond ((or (> col row) (< row 1)) 0)
	((= col 1) 1)
	((= row col) 1)
	(t (+ (pascals-tri (- row 1) (- col 1))
	      (pascals-tri (- row 1) col)))))

;Exercise 1.13 no thanks
;Exercise 1.14 no thanks

;Exercise 1.15
(defparameter *counter-1.15* 0)

(defun func-1.15 (x)
  (incf *counter-1.15*)
  (- (* 3 x) (* 4 (* x x x))))

(defun sine-1.15 (angle)
  (if (not (> (abs angle) 0.1))
      angle
      (func-1.15 (sine-1.15 (/ angle 3.0)))))

;a) Evaluated 5 times when angle -> 12.15
;b) on step n, angle will have been reduced by 3^n, so
;number of evaluations grow on the order of log base 3

;Exercise 1.16
(defun odd? (x)
  (= (mod x 2) 1))

(defun fast-expt-iter (a b n)
  (cond ((= n 0) a)
	((odd? n) (fast-expt-iter (* a b) b (- n 1)))
	(t (fast-expt-iter a (square b) (/ n 2)))))

(defun fast-expt (b n)
  (fast-expt-iter 1 b n))

;Exercise 1.17

(defun my-half (x)
  (/ x 2))

(defun my-double (x)
  (* x 2))

(defun fast-mult-rec (f s)
  (cond ((or (= f 0) (= s 0)) 0)
	((= s 1) f)
	((odd? s) (+ f (fast-mult-rec f (- s 1))))
	(t (fast-mult-rec (my-double f) (my-half s)))))

;Exercise 1.18
; Again the key is to pay attention to invariants!
(defun fast-mult-iter (sum f s)
  (cond ((or (= f 0) (= s 0)) 0)
	((= s 1) (+ sum f))
	((odd? s) (fast-mult-iter (+ sum f) f (- s 1)))
	(t (fast-mult-iter sum (my-double f) (my-half s)))))

(defun fast-mult (f s)
  (fast-mult-iter 0 f s))

;Exercise 1.19
;See hand worked derivation for how I got here

(defun even? (x)
  (= (mod x 2) 0))

(defun fib-iter (a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a b (+ (square p) (square q))
		   (+ (* 2 q p) (square q)) (/ count 2)))
	(t (fib-iter (+ (* b q) (* a q) (* a p))
		     (+ (* b p) (* a q))
		     p q (- count 1)))))

(defun my-fib (n)
  (fib-iter 1 0 0 1 n))

;Exercise 1.20
; Maybe later

;Exercise 1.21
;using functions
;199 smallest is 199
;1999 smallest is 1999
;19999 smallest is 7
(defun divides? (a b)
  (= (mod b a) 0))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(t (find-divisor n (+ test-divisor 1)))))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun prime? (n)
  (= (smallest-divisor n) n))

;Exercise 1.22
;CL makes this a bit easier (time (search-for-primes next num nil))

(defun search-for-primes (next num primes)
  (cond ((= num 0) primes)
	((prime? next) (search-for-primes (+ 2 next) (- num 1) (cons next primes)))
	(t (search-for-primes (+ 2 next) num primes))))
	 
;I had to up the base numbers to get any reasonable numbers
;1) 10000001: 0.001 seconds of real time
;2) 100000001: 0.003 seconds of real time
;3) 1000000001: 0.042 seconds of real time
;The time between 1) and 2) support the (sqrt 10) factor increase in time
;spent doing computations.  2) and 3) do now.  3) took increased by much
;way more than (sqrt 10).  This is because 3) was consing a lot more
;data which is a lot slower than crunching numbers.  So, the run time
;is not proportional to the number of steps, other factors are involved.


;Exercise 1.23
(defun next-1.23 (n)
  (cond ((= n 2) 3)
	(t (+ n 2))))

(defun find-divisor-1.23 (n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(t (find-divisor n (next-1.23 test-divisor)))))

(defun smallest-divisor-1.23 (n)
  (find-divisor-1.23 n 2))

(defun prime-1.23? (n)
  (= (smallest-divisor-1.23 n) n))

(defun search-for-primes-1.23 (next num primes)
  (cond ((= num 0) primes)
	((prime-1.23? next) (search-for-primes-1.23 (+ 2 next) (- num 1) (cons next primes)))
	(t (search-for-primes-1.23 (+ 2 next) num primes))))
	 
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

;Exercise 1.27
(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (mod (square (expmod base (/ exp 2) m)) m))
	(t (mod (* base (expmod base (- exp 1) m)) m))))
	
(defun try-it (n a)
  (= (expmod a n n) a))

(defun fermat-test (n)
  (try-it n (+ 1 (random (- n 1)))))

(defun fast-prime? (n times)
  (cond ((= times 0) t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(t nil)))

(defun carmichael-iter (n a)
  (cond ((= a n) t)
	((not (try-it n a)) a)
	(t (carmichael-iter n (+ a 1)))))

(defun carmichael-test (n)
  (carmichael-iter n 2))

;yep carmichael-test says they are prime when they are not

;Exercise 1.29
(defun my-sum (term a next b)
  (if (> a b)
      0
      (+ (funcall term a)
	 (my-sum term (funcall next a) next b))))

(defun inc (x) (+ x 1))

(defun integral (f a b dx)
  (defun add-dx (x) (+ x dx))
  (* (my-sum f (+ a (/ dx 2.0)) #'add-dx b)
     dx))

(defun simpson-h (n a b)
  (/ (- b a) n))

(defun simpson-multiplier (n k)
  (cond ((or (= k 0) (= k n)) 1.0)
	((odd? k) 4.0)
	(t 2.0)))
	 
(defun simpson-sum (func n a b k)
  (cond ((> k n) 0)
	(t (+ (* (simpson-multiplier n k) (funcall func (+ a (* k (simpson-h n a b)))))
	      (simpson-sum func n a b (+ k 1))))))

(defun simpson-integral (func n a b)
  (* (/ (simpson-h n a b) 3.0) (simpson-sum func n a b 0)))

(defun simpson-integral-2 (func n a b)
  (defun h () (/ (- b a) n))
  (defun multiplier (k)
    (cond ((or (= k 0) (= k n)) 1.0)
	  ((odd? k) 4.0)
	  (t 2.0)))
  (defun sarg (k) (+ a (* k (h))))
  (defun term (k) (* (multiplier k) (funcall func (sarg k))))
  (* (/ (h) 3) (my-sum #'term 0 #'inc n)))
  
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

;The reason I did not realize this it makes use of both lexical scoping
;and composing functions out of other functions.  This is simply not a 
;combination that one sees much outside of the Lisp community, so it
;takes some getting used to.

;Exercise 1.30
(defun my-sum-i (term a next b)
  (defun iter (a result)
    (if (> a b) result
	(iter (funcall next a) (+ result (funcall term a)))))
  (iter a 0))

;Exercise 1.31
;recursive
(defun product (func next a b)
  (if (> a b)
      1
      (* (funcall func a) (product func next (funcall next a) b))))

;iterative
(defun product-i (func next a b)
  (defun iter (a result)
    (if (> a b)
	result
	(iter (funcall next a) (* result (funcall func a)))))
  (iter a 1))

(defun factorial-w-product (a b)
  (product #'identity #'inc a b))

(defun pi-using-product (n)
  (defun next-frac (a)
    (/ (* (+ a 1) (+ a 3)) (square (+ a 2))))
  (defun next-term (a) (+ a 2))
  (* 4.0 (product #'next-frac #'next-term 1 n)))

;Exercise 1.32
(defun my-accumulate (combiner null-value func next a b)
  (if (> a b)
      null-value
      (funcall combiner (funcall func a)
	       (my-accumulate combiner null-value func next (funcall next a) b))))

(defun my-accumulate-i (combiner null-value func next a b)
  (defun iter (a result)
    (if (> a b)
	result
	(iter (funcall next a)
	      (funcall combiner result (funcall func a)))))
  (iter a null-value))

(defun sum-with-accumulate (func next a b)
  (my-accumulate #'+ 0 func next a b))

(defun product-with-accumulate (func next a b)
  (my-accumulate #'* 1 func next a b))

(defun sum-with-accumulate-i (func next a b)
  (my-accumulate-i #'+ 0 func next a b))

;Exercise 1.33
(defun filtered-accumulate (filter combiner null-value func next a b)
  (cond ((> a b) 
	 null-value)
	((funcall filter a) 
	 (funcall combiner (funcall func a)
		  (filtered-accumulate filter combiner null-value func next (funcall next a) b)))
	(t (filtered-accumulate filter combiner null-value func next (funcall next a) b))))

(defun sum-of-square-of-primes (lower upper)
  (filtered-accumulate #'prime? #'+ 0 #'square #'inc lower upper))

(defun product-gcd (n)
  (defun filter (x) (= 1 (gcd n x)))
  (filtered-accumulate #'filter #'* 1 #'identity #'inc 2 n))

;Exercise 1.34
;you get an error, because 2 is not a function
(defun f (g)
  (funcall g 2)) 

;Exercise 1.35
;Divide both sides of the equation on p 38 by phi and you get the fixed point function
(defparameter *tolerance* 0.00001)

(defun close-enough? (first second)
  (< (abs (- first second)) *tolerance*))

(defun fixed-point (f first-guess)
  (defun try (guess)
    (let ((next (funcall f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) in SLIME
;(fixed-point (lambda (x) (average (+ 1 (/ 1 x)) x)) 1.0) (with dampening)

;Exercise 1.36
(defun fixed-point-instr (f first-guess)
  (defun try (guess)
    (let ((next (funcall f guess)))
      (format t "guess is: ~d~&" next)
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

;(fixed-point-instr (lambda (x) (/ (log 1000) (log x))) 2.0) 34 tries, no damping
;(fixed-point-instr (lambda (x) (average (/ (log 1000) (log x)) x)) 2.0) 9 tries w/damping

;Exercise 1.37
(defun cont-frac (n d k)
  (defun next (i)
    (if (= i k) 
	(/ (funcall n i) (funcall d i))
	(/ (funcall n i) (+ (funcall d i) (next (+ i 1))))))
  (next 1))

;1.6180 -> Needs 13 calls to get golden ratio to 4 decimal places

(defun cont-frac-i (n d k)
  (defun next (i result)
    (if (= i 1) 
	(/ (funcall n 1) result)
	(next (- i 1) (+ (funcall d (- i 1)) (/ (funcall n i) result)))))
  (next k (funcall d k)))

;Exercise 1.38
(defun euler-d (n)
  (defun is-1 (step)
    (not (= (mod (+ step 1) 3) 0)))
  (defun euler-next (n step total)
    (cond ((> step n) total)
	  ((is-1 step) (euler-next n (+ step 1) total))
	  (t (euler-next n (+ step 1) (+ total 2)))))
  (if (is-1 n)
      1.0
      (euler-next n 1.0 0)))

(defun euler-e (n)
    (+ 2.0 (cont-frac (lambda (i) 1) #'euler-d n)))

;(euler-e 10) -> 2.7182817

;Exercise 1.39
(defun tan-cf (x k)
  (cont-frac (lambda (i)
	       (if (= i 1)
		   x
		   (* -1 (square x))))
	     (lambda (i) (- (* 2 i) 1))
	     k))

;Exercise 1.40
(defparameter *dx* 0.001)

(defun deriv (g)
  (lambda (x)
    (/ (- (funcall g (+ x *dx*)) (funcall g x)) *dx*)))

(defun newton-transform (g)
  (lambda (x)
    (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newtons-method (g guess)
  (fixed-point (newton-transform g) guess))

(defun sqrt-w-newton (x)
  (newtons-method (lambda (y) (- (square y) x))
		  1.0))

(defun cubic (a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

;Exercise 1.41
(defun double-call (g)
  (lambda (x)
    (funcall g (funcall g x))))

;(funcall (funcall (double-call (double-call #'double-call)) #'inc) 5) -> 21

;Exercise 1.42
(defun compose (f g)
  (lambda (x)
    (funcall f (funcall g x))))

;Exercise 1.43
(defun repeat-call (f n)
  (defun do-it (new-func i)
    (if (= i 0)
	new-func
	(do-it (compose f new-func) (- i 1))))
  (do-it f (- n 1)))

;Exercise 1.44 & 1.45 skip

;Exercise 1.46
(defun iterative-improve (good-enough? improve)
  (lambda (x)
    (defun try (guess)
      (if (funcall good-enough? guess x)
	  guess
	  (try (funcall improve guess x))))
    (try 1.0)))

(defun sqrt-iterative-improve (x)
  (funcall (iterative-improve (lambda (guess x)
				(< (abs (- (square guess) x)) 0.001))
			      (lambda (guess x)
				(average guess (/ x guess))))
	   x))
	   

(defun fixed-point-iterative-improve (f)
  (funcall (iterative-improve (lambda (guess f)
				(< (abs (- guess (funcall f guess))) *tolerance*))
			      (lambda (guess f)
				(funcall f guess)))
	   f))
	   