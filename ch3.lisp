;Ex 3.1
(defun make-accumulator (arg)
  (let ((counter arg))
    (lambda ()
      (incf counter))))

;Ex 3.2
(defun make-monitored (func)
  (let ((counter 0))
    (lambda (arg)
      (cond ((eq arg 'how-many-calls) counter)
	    ((eq arg 'reset-count) (setf counter 0))
	    (t (incf counter)
	       (funcall func arg))))))
	       
;Ex 3.3 Same concept as Ex 3.2
;Ex 3.4 Same concept as Ex 3.1, in fact just just make-accumulator

;Ex 3.8 CL evaluates from left to right
(defparameter *right-or-left-var* nil)

(defun right-or-left (num)
  (if (eq *right-or-left-var* nil) (setq *right-or-left-var* num))
  (cond ((eq *right-or-left-var* num) num)
	(t 0)))
