;Exercise 2.1
(defun make-rat (n d)
  (let ((g (gcd n d)))
    (cond ((or (and (> n 0) (> d 0))
	       (and (< n 0) (< d 0))) (cons (abs (/ n g)) (abs (/ d g))))
	  (t (cons (* -1 (abs (/ n g))) (abs (/ d g)))))))

(defun numer (x)
  (car x))

(defun denom (x)
  (cdr x))

(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun mult-rat (x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
	    (* (numer y) (denom x))))

(defun equal-rat? (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defun print-rat (x)
  (format t "~d/~d" (numer x) (denom x)))

;Exercise 2.2
(defun make-point (x y)
  (cons x y))

(defun x-point (point)
  (car point))

(defun y-point (point)
  (cdr point))

(defun print-point (point)
  (format t "(~d,~d)" (x-point point) (y-point point)))

(defun make-segment (start end)
  (cons start end))

(defun start-segment (segment)
  (car segment))

(defun end-segment (segment)
  (cdr segment))

(defun midpoint-segment (segment)
  (make-point (average (x-point (start-segment segment))
		       (x-point (end-segment segment)))
	      (average (y-point (start-segment segment))
		       (y-point (end-segment segment)))))

(defun print-segment (segment)
  (format t "Start: ")
  (print-point (start-segment segment))
  (format t "End: ")
  (print-point (end-segment segment)))

;Exercise 2.3
(defun length-segment (segment)
  (sqrt (+ (square (- (x-point (end-segment segment)) (x-point (start-segment segment))))
	   (square (- (y-point (end-segment segment)) (y-point (start-segment segment)))))))

(defun make-rect (left bottom)
  (cons left bottom))

(defun left-rect (rect)
  (car rect))

(defun bottom-rect (rect)
  (cdr rect))

(defun rect-perimeter (rect)
  (+ (* 2 (length-segment (left-rect rect)))
     (* 2 (length-segment (bottom-rect rect)))))

(defun rect-area (rect)
  (* (length-segment (left-rect rect))
     (length-segment (bottom-rect rect))))

(defun make-rect2 (corner top right)
  (cons corner (cons top right)))

(defun left-rect2 (rect)
  (make-segment (car rect) (car (cdr rect))))

(defun bottom-rect2 (rect)
  (make-segment (car rect) (cdr (cdr rect))))

;given proper name changes, they can use the same procedures for perimeter.
;However, to have a consistent interface you need some sort of either message passing
;or polymorphism

;Exercise 2.4
(defun my-cons (x y)
  (lambda (m) (funcall m x y)))

(defun my-car (z)
  (funcall z (lambda (p q) p)))

(defun my-cdr (z)
  (funcall z (lambda (p q) q)))

;Exercise 2.5
(defun cons-godel (a b)
  (lambda () 
    (* (expt 2 a) (expt 3 b))))

(defun num-mods (total base)
  (defun try (next i)
    (if (not (= 0 (mod next base)))
	i
	(try (/ next base) (+ i 1))))
  (try total 0))

(defun car-godel (z)
  (num-mods (funcall z) 2))

(defun cdr-godel (z)
  (num-mods (funcall z) 3))
    
;Exercise 2.6
;The basic idea is to use the lisp to build up a series
;of function calls which are delayed.  The translation comes
;in executing the delayed function calls and keep track of
;how often they are called.

;#1 I need to study up on computability theory
;#2 Scheme is syntactically much easier, lisp is more explicit
(defparameter *church-zero*
  (lambda (f)
    (lambda (x) x)))

(defparameter *church-one*
  (lambda (f)
    (lambda (x)
      (funcall f x))))

(defparameter *church-two*
  (lambda (f)
    (lambda (x)
      (funcall f (funcall f x)))))

(defparameter *church-three*
  (lambda (f)
    (lambda (x)
      (funcall f (funcall f (funcall f x))))))

(defun church-+ (a b)
  (lambda (f)
    (lambda (x) 
      (funcall (funcall a f) (funcall (funcall b f) x))))) ;<-- from Ken Dyck's site

(defun church-add-three (a b c)
  (lambda (f)
    (lambda (x)
      (funcall (funcall a f) (funcall (funcall b f) (funcall (funcall c f) x))))))

(defun church-* (a b)
  (defun do-it (total index)
    (if (church= index b)
	total
	(do-it (church-+ total a) (church-+ index *church-one*))))
  (cond ((church= b *church-zero*) *church-zero*)
	(t (do-it a *church-one*))))

(defun church= (a b)
  (= (church-translate a) (church-translate b)))

(defun church-translate (a)
  (funcall (funcall a #'inc) 0))
  
;Exercise 2.7
(defun make-interval (a b)
  (cons a b))

(defun upper-bound (interval)
  (cdr interval))

(defun lower-bound (interval)
  (car interval))

;Exercise 2.8
(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (lower-bound y))))

(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

;Exercise 2.9
;For addition and subtraction the new widths are in both
;cases wx + wy, where wx is the width of the first interval
;and wy is the width of the second interval.  For mult consider
;(1,5) and (1,20), the new width of 49.5 is not a function
;of the original widths.  Also intervals with the same original
;widths will not in general have the same widths after multiplication.
;The same holds for division

;Exercise 2.10
(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (if (and (> (upper-bound y) 0) (< (lower-bound y) 0))
      (error "Interval spans zero, will produce illegal interval")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
				     (/ 1.0 (lower-bound y))))))

;Exercise 2.11
;The main things I take from this are
;1) If you are going to optimize, you better make sure it is worth it
;because this is tedious work
;2) If you are going to do a case analysis, you better make sure
;it is comprehensive and it helps to have some sort of theoretical
;proof that you are handling everything.
;3) The following looks right, but becaue of 2) I would feel more
;comfortable NOT using it, but using the simpler and less efficient solution
(defun mul-interval-ben-bitdiddle (a b)
  (defun pos (x) (> x 0))
  (defun neg (x) (< x 0))
  (let ((al (lower-bound a))
	(au (upper-bound a))
	(bl (lower-bound b))
	(bu (upper-bound b)))

    (cond ((and (pos al) (pos au) (pos bl) (pos bu))
	   (make-interval (* al bl) (* au bu)))
	  
	  ((and (neg al) (pos au) (pos bl) (pos bu))
	   (make-interval (* al bu) (* au bu)))
	  
	  ((and (neg al) (neg au) (pos bl) (pos bu))
	   (make-interval (* al bu) (* au bl)))

	  ((and (pos al) (pos au) (neg bl) (pos bu))
	   (make-interval (* au bl) (* au bu)))
	  
	  ((and (neg al) (neg au) (neg bl) (pos bu))
	   (make-interval (* al bu) (* au bl)))

	  ((and (pos al) (pos au) (neg bl) (neg bu))
	   (make-interval (* au bl) (* al bu)))

	  ((and (neg al) (pos au) (neg bl) (neg bu))
	   (make-interval (* au bl) (* al bl)))

	  ((and (neg al) (neg au) (neg bl) (neg bu))
	   (make-interval (* au bu) (* al bl)))

	  (t (mul-interval a b)))))
	   

;Exercise 2.12
(defun make-center-width (c w)
  (make-interval (- c w) (+ c w)))

(defun make-center-percent (center percent)
  (make-interval (- center (* .01 percent center))
		 (+ center (* .01 percent center))))

(defun center (interval)
  (/ (+ (lower-bound interval) (upper-bound interval)) 2))

(defun width (interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2))

(defun percent (interval)
  (* 100 (/ (width interval) (center interval))))

;Exercise 2.13 see hand written work

;Exercise 2.14
;To be honest, I have no idea what the A/A and A/B stuff is supposed to show
(defun par1 (r1 r2)
  (div-interval (mul-interval r1 r2)
		(add-interval r1 r2)))

(defun par2 (r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
		  (add-interval (div-interval one r1)
				(div-interval one r2)))))

(defun print-center-percent (interval)
  (format t "Center: ~d, Percent: ~d" (center interval) (percent interval)))

;(print-center-percent (par1 (make-center-percent 5 0.5) (make-center-percent 10 0.5))) ->
;Center: 3.3446305, Percent: 1.1671897

;(print-center-percent (par2 (make-center-percent 5 0.5) (make-center-percent 10 0.5)))
;Center: 3.338852, Percent: 0.33389366

;The centers are fairly close, but the percentages are very different, 
;by a factor of 3.5

;Exercise 2.15
;Eva Lu Ator is correct.  When variables with uncertain numbers are repeated
;the uncertainties are magnified when any operation is performed on it, because
;by intervals must represent worst case scenarios.  Each worst case scenario
;builds on previous worst case scenarios and the error is magnified in the
;system as operations are performed.  par2 involves only one operation
;where uncertainties are magnified, the add-interval step.  For any
;given operation between two intervals, if one is exact then the % tolerance
;of the result will be the same as the % tolerance for the inexact interval.

;Exercise 2.16
;In general equivalent algebraic expressions lead to different answers because 
;in general computers have no notion of "equivalent algebraic expressions," indeed
;in general computers have no notion of "agebraic expression."  A procedure may
;correspond to an agebraic expression, but that procedure in general will not
;correspond to an equivalent agrebraic expression.  Two procedures will
;give rise to two different computational processes that will lead to different results, though
;they are related to equivalent algebraic properties.  The interval arithmetic
;package would have to do these things
;1) Represent intervals as algebraic structures, not computational procedures
;2) Be able to automatically transform algebraic structures into computational procedures
;3) Be able to transform algebraic structures into equivalent algebraic structures
;4) Search the space of equivalent algebraic structures for the corresponding 
;procedure which minimizes the calculated error

;Can this be done.  The search space of equivalent algebraic structures
;is infinite, so the steps cannot be done exactly.  Perhaps an inexact heuristic
;procedure could minimize the error in the calculations.

;Exercise 2.17
(defun last-pair (list)
  (if (null (cdr list))
      (car list)
      (last-pair (cdr list))))

;Exercise 2.18
(defun list-reverse (list)
  (defun do-it (old-list new-list)
    (cond ((null old-list) nil)
	  ((null (cdr old-list)) (cons (car old-list) new-list))
	  (t (do-it (cdr old-list) (cons (car old-list) new-list)))))
  (do-it list nil))

(defun list-reverse-r (l)
  (if (null l)
      nil
      (append (list-reverse-r (cdr l))
	      (list (car l)))))
      
;Exercise 2.19
;Maybe later, I don't feel like typing in the original program

;Exercise 2.20
;The equivalent to the . notation in CL is to use &rest
;I tried to make a procedure that generates a recursive process
;calling only same-parity.  The problem is that you are not
;appending on each call so you can't do something like:
;(cons (car others) (same-parity first (cdr others))) when
;you are not consing the car of others (because it doesn't belong
(defun same-parity (first &rest others)
  (defun do-it (list answer)
    (cond ((null (cdr list)) answer)
	  
	  ((or (and (odd? first) (odd? (car list)))
	       (and (even? first) (even? (car list))))
	   (do-it (cdr list) (append answer (list (car list)))))

	  (t (do-it (cdr list) answer))))
  (do-it others nil))

;Exercise 2.21
(defun square-list-cons (items)
  (if (null items)
      nil
      (cons (square (car items)) (square-list-cons (cdr items)))))

(defun square-list-map (items)
  (map 'list #'square items))

;Exercise 2.22
;Why is it reversing?  Take a look at the answer to 2.18,
;he is using my reverse implementation.  In the second try
;he simple creates invalid list structure

;Exercise 2.23
(defun my-for-each (func list)
  (if (not (null list))
      (progn
	(funcall func (car list))
	(my-for-each func (cdr list)))))

;July 6, 2008
;Exercise 2.24
;See work for box/pointer and tree
;(list 1 (list 2 (list 3))) -> (1 (2 3) 4)

;Exercise 2.25
;a) (car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9)))))) -> 7
;b) (car (car (list (list 7))))
;c) (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))))))))))) -> 7

;Exercise 2.26
;a) (append x y) -> (1 2 3 4 5 6)
;b) (cons x y) -> ((1 2 3) 4 5 6)
;c) (list x y) -> ((1 2 3) (4 5 6))

;Exercise 2.27
;With help from Eli Bendersky
(defun deep-reverse (tree)
  (cond ((null tree) nil)
	((atom (car tree)) (append (deep-reverse (cdr tree)) (list (car tree))))
	(t (append (deep-reverse (cdr tree)) (list (deep-reverse (car tree)))))))


;Exercise 2.28
;After spilling lots of my own blood attempting to understand 2.27
;(it took me a couple of hours of puzzling at least, I finished fringe
;in 5 minutes.  At least I have that consolation!
(defun fringe (tree)
  (cond ((null tree) nil)
	((atom (car tree)) (append (list (car tree)) (fringe (cdr tree))))
	(t (append (fringe (car tree)) (fringe (cdr tree))))))

;Exercise 2.29
(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
 (list length structure))

(defun left-branch (mobile)
  (car mobile))

(defun right-branch (mobile)
  (car (cdr mobile)))

(defun branch-length (branch)
  (car branch))

(defun branch-structure (branch)
  (car (cdr branch)))

(defun branch-weight (branch)
  (if (atom (branch-structure branch))
      (branch-structure branch)
      (+ 0 (total-weight (branch-structure branch)))))

(defun total-weight (mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(defun branch-torque (branch)
  (if (atom (branch-structure branch))
      (* (branch-length branch) (branch-weight branch))
      (* (branch-length branch) (total-weight (branch-structure branch)))))

(defun branch-balanced? (branch)
  (if (atom (branch-structure branch))
      t
      (balanced? (branch-structure branch))))

(defun balanced? (mobile)
  (and (= (branch-torque (left-branch mobile))
	  (branch-torque (right-branch mobile)))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))

;Changing the representation would require changing the constructors
;as well as the right-branch and branch-structure selectors

;July 7, 2008
;Exercise 2.30
(defun square-tree (tree)
  (cond ((null tree) nil)
	((atom tree) (square tree))
	(t (cons (square-tree (car tree))
		 (square-tree (cdr tree))))))

(defun square-tree-map (tree)
  (map 'list (lambda (sub)
	       (if (atom sub)
		   (square sub)
		   (square-tree-map sub))) tree))

;Exercise 2.31
(defun tree-map (func tree)
  (map 'list (lambda (sub)
	       (if (atom sub)
		   (funcall func sub)
		   (tree-map func sub))) tree))

(defun square-tree-generic (tree)
  (tree-map #'square tree))

;Exercise 2.32
;I mainly followed the algorithm for generating power sets
;on wikipedia.  The gist of the algorithm is that a the nth
;power set it just the (n-1)th powerset, along with n appended
;to each member of the (n-1)th power set.  The base case is nil, 
;which only has nil as a member of its power set.  
;So the 1st power set will be nil and 1 appended to nil, which
;gives (nil (1)).  The second poweset will be (nil (2) (1) (2 1)) etc. 
(defun subsets (s)
  (if (null s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map 'list (lambda (next)
				  (append next (list (car s)))) rest)))))

;Exercise 2.33
;cl calls accumulate reduce, but works differently.  Reduce
;is much more natural for iterative style work.  The function
;is called in order and the accumlating result is passed in
;the first arg after the first invocation, which means it evolves
;an iterative process.  The scheme style accumulate evolves a
;recursive process, which means you have to think the opposite
;direction as when using the CL reduce function.  It's helpful
;to draw a picture when using the Scheme style accumulate.
(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (car sequence)
	       (accumulate op initial (cdr sequence)))))

(defun filter (predicate sequence)
  (cond ((null sequence) nil)
	((funcall predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(t (filter predicate (cdr sequence)))))

(defun map-using-accumulate (func sequence)
  (accumulate (lambda (x y)
		(cons (funcall func x) y)) nil sequence))

(defun map-using-reduce (func sequence)
  (reduce (lambda (x y)
	    (if (listp x)
		(append x (list (funcall func y)))
		(list (funcall func x) (funcall func y)))) sequence))

(defun append-using-accumulate (seq1 seq2)
  (accumulate #'cons seq2 seq1))

(defun length-using-accumulate (sequence)
  (accumulate (lambda (x y)
		(inc y)) 0 sequence))

;Exercise 2.34
(defun horner-eval (x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0 coefficient-sequence))

;Exercise 2.35
(defun count-leaves (tree)
  (accumulate #'+ 0
	      (map 'list (lambda (x) 1) (fringe tree))))

;July 8, 2008
;Exercise 2.36
;I originally wrote two functions, first and rest, which
;did what the two mapping sequences did.  While it worked
;it was not as "elegant" as the solution on Eli
;Bendersky's website.
(defun accumulate-n (op init seqs)
  (if (null (car seqs))
      nil
      (cons (accumulate op init (map 'list #'car seqs))
	    (accumulate-n op init (map 'list #'cdr seqs)))))

;Exercise 2.37
(defun dot-product (v w)
  (accumulate #'+ 0 (map 'list #'*  v w)))

(defun matrix-*-vector (m v)
  (map 'list (lambda (row)
	       (dot-product row v)) m))

(defun transpose (mat)
  (accumulate-n #'(lambda (x y) (cons x y)) nil mat))

(defun matrix-*-matrix (m n)
  (let ((cols (transpose n)))
    (map 'list (lambda (row)
		 (map 'list (lambda (col)
			      (dot-product row col)) cols)) m)))
	       

;Exercise 2.38
(defun fold-right (op initial sequence)
  (accumulate op initial sequence))

(defun fold-left (op initial sequence)
  (defun iter (result rest)
    (if (null rest)
	result
	(iter (funcall op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

;(fold-right #'/ 1 (list 1 2 3)) -> 3/2
;(fold-left #'/ 1 (list 1 2 3)) -> 1/6
;(fold-right #'list nil (list 1 2 3)) -> (1 (2 (3 NIL)))
;(fold-left #'list nil (list 1 2 3)) -> (((NIL 1) 2) 3)

;The operation must be commutative to produce the same values for any sequence

;Exercise 2.39
;I just guessed on these ones until I got it right.  I knew it had to be
;either cons or append I was looking for.  I need to work through it to
;see what is going on.
(defun reverse-fold-right (sequence)
  (fold-right (lambda (x y)
		(append y (list x))) nil sequence))

(defun reverse-fold-left (sequence)
  (fold-left (lambda (x y)
	       (cons y x)) nil sequence))

;July 9, 2008
;Exercise 2.40
(defun enumerate-interval (lower upper)
  (if (> lower upper)
      nil
      (cons lower (enumerate-interval (+ lower 1) upper))))

(defun flatmap (proc seq)
  (accumulate #'append nil (map 'list proc seq)))

(defun unique-pairs (n)
  (flatmap (lambda (x)
	     (map 'list (lambda (y)
			  (list x y)) (enumerate-interval (+ x 1) n)))
	   (enumerate-interval 1 n)))

(defun prime-sum? (pair)
  (prime? (+ (car pair) (cadr pair))))

(defun make-pair-sum (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(defun prime-sum-pairs (n)
  (map 'list #'make-pair-sum
       (filter #'prime-sum? (unique-pairs n))))

;Exercise 2.41
(defun ordered-triples (n)
  (flatmap (lambda (x)
	     (map 'list (lambda (y)
			  (append x (list y))) (enumerate-interval (+ (cadr x) 1) n)))
	   (unique-pairs (- n 1))))

;At least for now I will be skipping 2.42 to 2.52 inclusive.
;2.42 & 2.43 look interesting, but also look rather time consuming
;I won't be able to check my work for 2.44 to 2.52 unless I had
;the picture language compiled in a scheme system, which I don't.
;I also don't feel like going through the hassle of setting all of
;that up.

;Exercise 2.53
(defun memq (item x)
  (cond ((null x) nil)
	((eq item (car x)) x)
	(t (memq item (cdr x)))))

;(list 'a 'b 'c) -> (A B C)
;(list (list 'george)) -> ((GEORGE))
;(cdr '((x1 x2) (y1 y2))) -> ((Y1 Y2))
;(cadr '((x1 x2) (y1 y2))) -> (Y1 Y2)
;(listp (car '(a short list))) -> NIL
;(memq 'red '((red shoes) (blue socks))) -> NIL
;(memq 'red '(red shoes blue socks)) -> (RED SHOES BLUE SOCKS)

;Exercise 2.54
(defun equal? (one two)
  (cond ((or (and (null one) (not (null two)))
	     (and (not (null one)) (null two))) nil)

	((and (null one) (null two)) t)

	((and (atom one) (atom two) (eq one two)) t)
	
	((and (listp one) (listp two))
	 (and (eq (car one) (car two)) (equal? (cdr one) (cdr two))))

	(t nil)))

;Exercise 2.55
;' is simply shorthand for the quote macro.  So you are quoting a quote
;which just returns the symbol for the quote macro, "quote."  Got it?

;Exercises 2.56 & 2.57
;Note, allowing for arbitrary products makes simplification of
;ones much more complicated.
(defun number? (n)
  (typep n 'number))

(defun =number? (one two)
  (and (number? one) (number? two) (= one two)))

(defun variable? (v)
  (typep v 'symbol))

(defun same-variable? (v1 v2)
  (and (variable? v1) (variable? v2) (eq v1 v2)))

(defun all-numbers? (l)
  (cond ((null l) nil)
	((null (cdr l)) (number? (car l)))
	(t (and (number? (car l)) 
		(all-numbers? (cdr l))))))

(defun make-sum (a1 &rest a2)
  (cond ((all-numbers? (append (list a1) a2))
	 (apply '+ (append (list a1) a2)))
	
	((null (cdr a2)) (list '+ a1 (car a2)))
	
	(t (list '+ a1 (apply #'make-sum (append (list (car a2)) (cdr a2)))))))

(defun make-product (m1 &rest m2)
  (cond ((all-numbers? (append (list m1) m2)) 
	 (apply '* (append (list m1) m2)))

	((=number? m2 1) m1)

	((=number? m1 1)
	 (cond ((= (length m2) 1) (car m2))
	       (t (apply #'make-product (append (list (car m2)) (cdr m2))))))
	
	((null (cdr m2)) (list '* m1 (car m2)))
	
	(t (list '* m1 (apply #'make-product (append (list (car m2)) (cdr m2)))))))

(defun sum? (x)
  (and (listp x) (eq (car x) '+)))

(defun addend (x)
  (cadr x))

(defun augend (x)
  (if (null (cdddr x))
      (caddr x)
      (apply #'make-sum (append (list (caddr x)) (cdddr x)))))

(defun product? (x)
  (and (listp x) (eq (car x) '*)))

(defun multiplier (p)
  (cadr p))

(defun multiplicand (p)
  (if (null (cdddr p))
      (caddr p)
      (apply #'make-product (append (list (caddr p)) (cdddr p)))))

(defun exponentiation? (x)
  (and (listp x) (eq (car x) '**)))

(defun base (x)
  (cadr x))

(defun exponent (x)
  (caddr x))

(defun make-exponent (e1 e2)
  (cond ((=number? e2 0) 1)
	((=number? e2 1) e1)
	(t (list '** e1 e2))))

(defun deriv (expression var)
  (cond ((number? expression) 0)

	((variable? expression)
	 (if (same-variable? expression var) 1 0))

	((sum? expression)
	 (make-sum (deriv (addend expression) var)
		   (deriv (augend expression) var)))

	((product? expression)
	 (make-sum (make-product (multiplier expression)
				 (deriv (multiplicand expression) var))
		   (make-product (deriv (multiplier expression) var)
				 (multiplicand expression))))

	((exponentiation? expression)
	 (make-product
	  (make-product (exponent expression)
			(make-exponent (base expression) (- (exponent expression) 1)))
	  (deriv (base expression) var)))

	(t (error "unknown expression type -- deriv"))))

;July 12, 2008
;Exercise 2.58
;a) With the simplifications all you need to do is change
;make-product and make-sum to put the op in the middle
;and then addend/augend/multiplier/multiplicand can pull out
;the first and third elements, instead of the second and third
;elements
;b) Yes it is much harder and I won't be doing this problem.

;Exercise 2.59
(defun element-of-set? (x set)
  (cond ((null set) nil)
	((equal x (car set)) t)
	(t (element-of-set? x (cdr set)))))

(defun union-set (set1 set2)
  (cond ((null set1) set2)
	((null set2) set1)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(t (union-set (cdr set1) (cons (car set1) set2)))))

;Exercise 2.60
;element-of-set? and intersection-set are the same
(defun adjoin-set-with-dups (x set)
  (cons x set))

(defun union-set-with-dups (set1 set2)
  (append set1 set2))
  
;The efficiency of adjoin-set and union-set are both O(1), before they
;were O(n) and O(n^2) respectively.  Element-of-set? and
;intersection-set will both be the same as before, plus a constant
;factor (the number of duplicates).  If you are going to adjoin and union
;a lot and the number of expected duplicates is low then it will be
;worth it to allow for duplicates, assuming your application
;is fault tolerant in the face of duplicates.

;Exercise 2.61
(defun element-of-sorted-set? (x set)
  (cond ((null set) nil)
	((= x (car set)) t)
	((< x (car set)) nil)
	(t (element-of-sorted-set? x (cdr set)))))

(defun insert-sorted-set (x set)
  (defun iter-insert (new-set remaining)
    (cond ((null remaining) (append new-set (list x)))
	  ((> (car remaining) x) (append new-set (list x) remaining))
	  (t (iter-insert (append new-set (list (car remaining))) (cdr remaining)))))

  (if (null set)
      (list x)
      (iter-insert nil set)))

(defun insert-sorted-set-r (x set)
  (cond ((null set) (list x))
	((< x (car set)) (append (list x) set))
	(t (append (list (car set)) (insert-sorted-set-r x (cdr set))))))
  
(defun adjoin-sorted-set (x set)
  (if (element-of-sorted-set? x set)
      set
      (insert-sorted-set x set)))

;Exercise 2.62
(defun union-sorted-set (set1 set2)
  (defun iter-union (new-set one two)
    (cond ((null one) (append new-set two))
	  ((null two) (append new-set one))
	  ( (= (car one) (car two)) (iter-union (append new-set (list (car one))) (cdr one) (cdr two)))
	  ( (< (car one) (car two)) (iter-union (append new-set (list (car one))) (cdr one) two))
	  ( (< (car two) (car one)) (iter-union (append new-set (list (car two))) one (cdr two)))))

  (cond ((null set1) set2)
	((null set2) set1)
	(t (iter-union nil set1 set2))))

;Exercises 2.63 -> 2.72 I will skip for now.  Both subjects are going to come up
;in a data structures/algorithms text which I will be studying shortly.  I feel
;pretty comfortable with Lisp/Scheme now so I really don't feel the need
;to slog through these pages.