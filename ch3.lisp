(defpackage :ch3 (:use :cl) (:export :monte-carlo :estimate-pi :cesaro-test))
(in-package :ch3)
(defun monte-carlo (trials experiment)
  (loop for i from 0 below trials
	summing (if (funcall experiment) 1 0) into success
	finally (return (/ success trials))))

(defun cesaro-test ()
  (= (gcd (random 100000) (random 100000)) 1))

(defun estimate-pi (trials)
  (sqrt (/ 6 (monte-carlo trials #'cesaro-test))))

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

(defpackage :ex3.8 (:use :cl) (:export :f))
(in-package :ex3.8)

(let ((is-set nil)
      (val 0))
  (defun f (v)
    (if is-set
	val
	(setf is-set t val v))))
