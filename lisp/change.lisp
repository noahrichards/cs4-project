(in-package :com.noahsmark.cs4project)

(defclass change ()
  ((start
    :initarg :start
    :initform (error "Must specify starting value.")
    :reader start
    :documentation "Starting amount to make change for.")
   (denoms
    :initarg :denoms
    :initform (error "Must specify at least one denomination.")
    :reader denoms
    :documentation "Denominations of change to make.")))

(defmethod init-state ((change change))
  (start change))

(defmethod is-end? ((change change) state)
  (= state 0))

(defmethod get-neighbors ((change change) state)
  (labels
      ((genlist (amnts)
	 (cond
	   ((endp amnts) nil)
	   ((>= (- state (car amnts)) 0)
	    (cons (- state (car amnts)) (genlist (cdr amnts))))
	   (t (genlist (cdr amnts))))))
    (genlist (denoms change))))


(deftest test-change ()
  (check
   (equalp (solve (make-instance 'change :start 0 :denoms '(10 23 30)))
	   '(0))                               ;init state is answer
   (equalp (solve (make-instance 'change :start 10 :denoms '(4 1 3 2 5)))
	   '(10 5 0))                          ;fastest move isn't first denom
   (equalp (solve (make-instance 'change :start 30 :denoms '(17 5 1)))
	   '(30 13 8 3 2 1 0))                 ;uses all moves
   (eq     (solve (make-instance 'change :start 100 :denoms '(99 3)))
	   nil)))                              ;unsolvable
