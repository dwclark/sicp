(defpackage :ch3 (:use :cl) (:export :monte-carlo :estimate-pi :cesaro-test
				     :make-queue :front-queue :insert-queue :delete-queue :empty-queue-p))
(in-package :ch3)
(defun monte-carlo (trials experiment)
  (loop for i from 0 below trials
	summing (if (funcall experiment) 1 0) into success
	finally (return (/ success trials))))

(defun cesaro-test ()
  (= (gcd (random 100000) (random 100000)) 1))

(defun estimate-pi (trials)
  (sqrt (/ 6 (monte-carlo trials #'cesaro-test))))

(defun empty-queue-p (q)
  (null (car q)))
  
(defun make-queue ()
  (cons nil nil))

(defun front-queue (q)
  (if (empty-queue-p q)
      (error "front called with empty queue")
      (car (car q))))

(defun insert-queue (q item)
  (let ((new-pair (cons item nil)))
    (cond ((empty-queue-p q)
	   (rplaca q new-pair)
	   (rplacd q new-pair)
	   q)
	  (t
	   (rplacd (cdr q) new-pair)
	   (rplacd q new-pair)
	   q))))

(defun delete-queue (q)
  (cond ((empty-queue-p q)
	 (error "delete-queue called with empty queue"))
	(t
	 (rplaca q (cdr (car q)))
	 q)))

(defpackage :ex3.1 (:use :cl) (:export :make-accumulator))
(in-package :ex3.1)
(defun make-accumulator (start)
  (let ((current start))
    (lambda (by)
      (incf current by))))

(defpackage :ex3.2 (:use :cl) (:export :make-monitored))
(in-package :ex3.2)
(defun make-monitored (func)
  (let ((counter 0))
    (lambda (arg)
      (case arg
	(:how-many-calls counter)
	(:reset-count (setf counter 0))
	(otherwise (incf counter)
	 (funcall func arg))))))

(defpackage :ex3.3 (:use :cl) (:export :make-account))
(in-package :ex3.3)

(defun make-account (balance &key password)
  (let ((acct-password password))
    (flet ((account-action (amount &key action password)
	     (if (not (eq password acct-password))
		 (return-from account-action "incorrect password"))
	     
	     (case action
	       (:withdraw (if (>= balance amount)
			      (decf balance amount)
			      "Insufficient funds"))
	       (:deposit (incf balance amount))
	       (otherwise (error (format t "unknown request ~A" action))))))
      #'account-action)))

(defpackage :ex3.4 (:use :cl) (:export :make-account))
(in-package :ex3.4)
(defun call-the-cops () "We called the cops on you")

(defun make-account (balance &key password)
  (let ((acct-password password)
	(bad-tries 0))
    (labels ((failed ()
	       (incf bad-tries)
	       (if (>= bad-tries 7)
		   (call-the-cops)
		   "incorrect password"))
	     
	     (account-action (amount &key action password)
	       (if (eq password acct-password)
		   (progn
		     (setf bad-tries 0)
		     (case action
		       (:withdraw (if (>= balance amount)
				      (decf balance amount)
				      "Insufficient funds"))
		       (:deposit (incf balance amount))
		       (otherwise (error (format t "unknown request ~A" action)))))
		   (failed))))
		   
      #'account-action)))

(defpackage :ex3.5 (:use :cl :ch3) (:export :estimate-integral))
(in-package :ex3.5)

(defun estimate-integral (trials pred x1 x2 y1 y2)
  (let ((x-width (- x2 x1))
	(y-width (- y2 y1)))
    (flet ((test ()
	     (let ((x (+ x1 (random x-width)))
		   (y (+ y1 (random y-width))))
	       (funcall pred x y))))
      (* x-width y-width (monte-carlo trials #'test)))))

(defun simple-circle (x y)
  (<= (+ (expt (- x 5) 2) (expt (- y 7) 2)) 9))
	     
(defpackage :ex3.6 (:use :cl) (:export :with-consistent-random))
(in-package :ex3.6)
;;different from what was called for, but better use of common-lisp
;;will make it so that any call to random inside func will give
;;the same sequence of calls
(defun with-consistent-random (state func)
  (let ((*random-state* (make-random-state state)))
    (funcall func)))

(defpackage :ex3.7 (:use :cl :ex3.3) (:export :joint-account))
(in-package :ex3.7)
;; used the same way as the original, but now you specify
;; the new password to the joint account for example:
;; (defparameter peter (ex3.3:make-account 100 :password :peter-password))
;; (defparameter paul (make-joint peter :original :peter-password :new-password :paul-password))
;; then, the following calls do the same thing
;; (funcall paul 50 :action :withdraw :password :paul-password)
;; (funcall peter 50 :action :withdraw :password :peter-password)
(defun make-joint (acct &key original new-password)
  (flet ((account-action (amount &key action password)
	   (if (eq password new-password)
	       (funcall acct amount :action action :password original)
	       "incorrect-password")))
    #'account-action))
;; Note, can also make this more scheme-like by doing the following (inside this package)
;; (setf (symbol-function 'peter) (ex3.3:make-account 100 :password :peter-password))
;; (setf (symbol-function 'paul) (make-joint #'peter :original :peter-password :new-password :paul-password))
;; (peter 50 :withdraw :password :peter-password)
;; (paul 25 :action :withdraw :password :paul-password)
;; (peter 500 :action :deposit :password :peter-password)

(defpackage :ex3.8 (:use :cl) (:export :f))
(in-package :ex3.8)

(let ((is-set nil)
      (val 0))
  (defun f (v)
    (if is-set
	val
	(setf is-set t val v))))

;; ex 3.9 This is writing diagrams. I understand the environment model so I'm going to bypass this as it's
;; also a pain to try and reproduce this in comments/text. Besides I don't really find the diagrams all
;; that instructive. It's hard to diagram an evolving process with static diagrams. In any case, the whole
;; point is to demonstrate the concepts of lexical scope, mutability, and shadowing.

;; ex3.10 same

;; ex3.11 same

(defpackage :ex3.12 (:use :cl) (:export :exec))
(in-package :ex3.12)

(defun exec ()
  (let* ((x (list 'a 'b))
	 (y (list 'c 'd))
	 (z (append x y)))
    (format t "z: ~A~%" z)
    (format t "(cdr x): ~A (should be (B)) ~%" (cdr x))

    (let ((w (nconc x y)))
      (format t "w: ~A~%" w)
      (format t "(cdr x): ~A (should be (B C D))" (cdr x)))))

;; ex3.13
;; It will look like a regular box and pointer diagram, except the cons cell with
;; C in the car will have a pointer in the cdr that points to the cons cell with
;; A in the car cell. If you try and compute last pair you will have an infinite
;; recursion.

(defpackage :ex3.14 (:use :cl) (:export :mystery))
(in-package :ex3.14)

;; mystery is an inplace reverse function
(defun mystery (x)
  (labels ((loopy (x y)
	     (format t "x: ~A, y: ~A~%" x y)
	     (if (null x)
		 y
		 (let ((temp (cdr x)))
		   (rplacd x y)
		   (loopy temp x)))))
    (loopy x nil)))

;; ex3.15
;; More diagramming. I get it, the cons cells are shared in z1, but not in z2

(defpackage :ex3.16 (:use :cl) (:export :count-pairs))
(in-package :ex3.16)

(defun count-pairs (x)
  (if (not (consp x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

;; (count-pairs (cons 1 (cons 2 (cons 3 nil)))) -> 3

#|
(count-pairs (let* ((x (cons nil nil))
	       (y (cons nil nil))
	       (z (cons 'a nil)))
	  (setf (car x) y)
	  (setf (car y) z)
	  (setf (cdr y) z)
x)) -> 4
|#

#|
(count-pairs (let* ((x (cons nil nil))
      (y (cons nil nil))
      (z (cons 'a nil)))
 (setf (car x) y)
 (setf (cdr x) y)
 (setf (car y) z)
 (setf (cdr y) z)
x)) ->7
|#

#|
(count-pairs (let ((lst (list 1 2 3)))
(setf (cdddr lst) lst))) -> infinite
|#

(defpackage :ex3.17 (:use :cl) (:export :count-pairs))
(in-package :ex3.17)

;; for all of the previous in ex 3.16, this returns 3
(defun count-pairs (x)
  (let ((seen nil))
    (labels ((inner (x)
	       (if (member x seen :test #'eq)
		   0
		   (progn
		     (push x seen)
		     (if (not (consp x))
			 0
			 (+ (inner (car x))
			    (inner (cdr x))
			    1))))))
      (inner x))))

(defpackage :ex3.18 (:use :cl) (:export :cyclic-p))
(in-package :ex3.18)

(defun cyclic-p (lst)
  (let ((seen nil))
    (labels ((test-for-cycle (cell)
	       (cond ((member cell seen :test #'eq) t)
		     ((null (cdr cell)) nil)
		     (t (push cell seen)
			(test-for-cycle (cdr cell))))))
      (test-for-cycle lst))))

(defpackage :ex3.19 (:use :cl) (:export :cyclic-p))
(in-package :ex3.19)

;; basic idea, in a non-cyclic list the number of pairs == length of list
;; if this is violated, then it is cyclic
(defun cyclic-p (lst)
  (let ((num-pairs (ex3.17:count-pairs lst)))
    (labels ((test-for-cycle (tmp so-far)
	       (cond ((null tmp) nil)
		     ((< num-pairs so-far) t)
		     (t (test-for-cycle (cdr tmp) (1+ so-far))))))
      (test-for-cycle lst 0))))

;; returns true for 
#|
(cyclic-p (let ((lst (list 1 2 3)))
(setf (cdddr lst) (cdar lst))))

and

(cyclic-p (let ((lst (list 1 2 3)))
(setf (cdddr lst) (cdr lst))))
|#

;; ex 3.20 More diagrams, I get it.

(defpackage :ex3.21 (:use :cl :ch3) (:export :print-qeue))
(in-package :ex3.21)

;; It's not printing correctly because the default print function for
;; a cons cell is being used. To make it print something better, just
;; take the car of the queue and print that since it's a list
(defun print-queue (q)
  (format t "~A~%" (car q)))

(defpackage :ex3.22 (:use :cl) (:export :make-queue))
(in-package :ex3.22)

(defun make-queue ()
  (let* ((front nil)
	 (rear nil))

    (labels ((empty-q ()
	       (null front))

	     (front-q ()
	       (if (empty-q)
		   (error "front called with empty queue")
		   (car front)))

	     (insert-q (item)
	       (cond ((empty-q)
		      (setf front (list item))
		      (setf rear front)
		      front)
		     (t
		      (let ((new-item (cons item nil)))
			(rplacd rear new-item)
			(setf rear new-item)
			front))))

	     (delete-q ()
	       (if (empty-q)
		   (error "delete called with empty queue")
		   (setf front (cdr front))))

	     (print-q ()
	       (format t "~A~%" front)))

      (lambda (&key (item nil) op)
	(case op
	  (:empty (empty-q))
	  (:front (front-q))
	  (:insert (insert-q item))
	  (:delete (delete-q))
	  (:print (print-q)))))))

(defpackage :ex3.23 (:use :cl) (:export :make-deque :empty-deque-p :front-deque :rear-deque
					:front-insert-deque :rear-insert-deque
					:front-delete-deque :rear-delete-deque :print-deque))
(in-package :ex3.23)

;; Basic representation: deque is a cons cell, the car contains the front of the deque
;; while the cdr contains the rear of the deque. The deque is a doubly linked list (DLL). Each
;; entry is a list. The (first entry) points to the previous entry in the DLL, while (third entry)
;; points to the next entry in the DLL. If (first entry) is nil, it's the first entry in the DLL,
;; and if (third entry) is nil, it's the last entry in the DLL.
(defun make-deque ()
  (cons nil nil))

(defun empty-deque-p (d)
  (null (car d)))

(defun front-deque (d)
  (if (empty-deque-p d)
      (error "front called with empty deque")
      (second (car d))))

(defun rear-deque (d)
  (if (empty-deque-p d)
      (error "rear called with empty deque")
      (second (cdr d))))

(defun front-insert-deque (d item)
  (let ((new (list nil item nil)))
    (cond ((empty-deque-p d)
	   (rplaca d new)
	   (rplacd d new)
	   :complete)
	  (t
	   (setf (first (car d)) new)
	   (setf (third new) (car d))
	   (rplaca d new)
	   :complete))))

(defun front-delete-deque (d)
  (cond ((empty-deque-p d)
	 (error "front-delete-deque called with empty deque"))
	((null (third (car d)))
	 (rplaca d nil)
	 (rplacd d nil)
	 :complete)
	(t
	 (rplaca d (third (car d)))
	 (setf (first (car d)) nil)
	 :complete)))

(defun rear-insert-deque (d item)
  (let ((new (list nil item nil)))
    (cond ((empty-deque-p d)
	   (rplaca d new)
	   (rplacd d new)
	   :complete)
	  (t
	   (setf (third (cdr d)) new)
	   (setf (first new) (cdr d))
	   (rplacd d new)
	   :complete))))

(defun rear-delete-deque (d)
  (cond ((empty-deque-p d)
	 (error "rear-delete-deque called with empty deque"))
	((null (first (cdr d)))
	 (rplaca d nil)
	 (rplacd d nil)
	 :complete)
	(t
	 (rplacd d (first (cdr d)))
	 (setf (third (cdr d)) nil)
	 :complete)))

(defun extract-list (d)
  (if (empty-deque-p d)
      nil
      (labels ((extract (cur so-far)
		 (if (null (third cur))
		     (reverse (cons (second cur) so-far))
		     (extract (third cur) (cons (second cur) so-far)))))
	(extract (car d) nil))))
      
(defun print-deque (d)
  (format t "~A~%" (extract-list d)))
      
;; ex 3.24: I'm not going to do this one, it's fairly obvious. Plus most of the stuff
;; for association lists already exists by default in common lisp, and I plan on using
;; those builtin features for the next exercise (and any other exercise requiring a-lists)

(defpackage :ex3.25 (:use :cl) (:export :lookup :insert :make-table))

(in-package :ex3.25)

(defun make-table ()
  (cons :header nil))

(defun lookup (table keys)
  (if (and (not (null table)) (consp (cdr table)))
      (let ((record (assoc (car keys) (cdr table))))
	(if record
	    (if (cdr keys)
		(lookup record (cdr keys))
		(cdr record))
	    nil))
      nil))

(defun insert (table keys value)
  (let ((record (assoc (car keys) (cdr table))))
    (if record
	(if (cdr keys)
	    (insert record (cdr keys) value)
	    (rplacd record value))
	(let ((new-record (cons (car keys) nil)))
	  (rplacd table (cons new-record (cdr table)))
	  (if (cdr keys)
	      (insert new-record (cdr keys) value)
	      (rplacd new-record value)))))
  :complete)

;; testing
;; (defparameter *t* (make-table))
;; (insert *t* '(a b c) 10)
;; (insert *t* '(d e f) 20)
;; (insert *t* '(x y) 30)
;; (lookup *t* '(a b c)) -> 10
;; (lookup *t* '(x y)) -> 30
;; (lookup *t* '(key)) -> nil
;; (lookup *t* '(x y z)) -> nil
