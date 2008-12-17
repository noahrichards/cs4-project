(in-package :com.noahsmark.cs4project)

(defclass clock ()
  ((size
    :initarg :size
    :initform 12
    :reader size
    :documentation "Size of the clock")
   (from
    :initarg :from
    :initform 2
    :reader from
    :documentation "Starting hour on the clock")
   (to
    :initarg :to
    :initform 4
    :reader to
    :documentation "Goal hour")))

(defmethod init-state ((clock clock))
  (from clock))

(defmethod is-end? ((clock clock) state)
  (= state (to clock)))

(defmethod get-neighbors ((clock clock) state)
  (let ((modstate (1- state)))
    (list
     (+ (mod (1- modstate) (size clock)) 1)
     (+ (mod (1+ modstate) (size clock)) 1)
     )))	


(deftest test-clock ()
  (check
   (equalp (solve (make-instance 'clock :size 12 :from 2 :to 4))
	   '(2 3 4))                       ;regular steps up
   (equalp (solve (make-instance 'clock :size 12 :from 4 :to 2))
	   '(4 3 2))                       ;regular steps down
   (equalp (solve (make-instance 'clock :size 12 :from 2 :to 2))
	   '(2))                           ;initial state is answer
   (equalp (solve (make-instance 'clock :size 12 :from 2 :to 9))
	   '(2 1 12 11 10 9))              ;wrap under bottom
   (equalp (solve (make-instance 'clock :size 30 :from 29 :to 7))
	   '(29 30 1 2 3 4 5 6 7))         ;wrap over top
   (eq     (solve (make-instance 'clock :size 12 :from 2 :to 13))
	   nil)))                          ;unsolvable
