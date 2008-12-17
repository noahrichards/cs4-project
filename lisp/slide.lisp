(in-package :com.noahsmark.cs4project)

(defun getxy (width row col)
    (+ (* row width) col))

(defclass slide ()
  ((start
    :initarg :start
    :initform (error "Must specify starting matrix.")
    :documentation "Starting matrix, as a list of rows (list of cols).")
   (goal
    :initarg :goal
    :initform (error "Must specify goal matrix.")
    :documentation "Goal matrix, as a list of rows (list of cols).")
   (start-arr
    :documentation "Internal array representation of start.")
   (goal-arr
    :documentation "Internal array representation of goal.")
   (width
    :documentation "Width of the slide puzzle state.")
   (height
    :documentation "Height of the slide puzzle state.")))

(defmethod initialize-instance :after ((slide slide) &key)
  (with-slots (start goal start-arr goal-arr width height) slide
    (let ((flatstart (apply #'append start))
	  (flatgoal (apply #'append goal)))
      (setf height (length start))
      (setf width (length (car start)))
      (setf start-arr
	    (make-array (length flatstart)
			:initial-contents flatstart))
      (setf goal-arr
	    (make-array (length flatgoal)
			:initial-contents flatgoal)))))
      
(defmethod init-state ((slide slide))
  (slot-value slide 'start-arr))

(defmethod is-end? ((slide slide) state)
  (equalp (slot-value slide 'goal-arr) state))

(defmethod get-neighbors ((slide slide) state)
  (with-slots (width height) slide
      (labels
	  ((can-move? (newrow newcol)
	     (not (or (< newrow 0)
		      (< newcol 0)
		      (>= newcol width)
		      (>= newrow height))))
	   (move (arr row col newrow newcol)
	     (let ((fromix (getxy width row col))
		   (toix (getxy width newrow newcol))
		   (newarr (copy-seq arr)))
	       (rotatef (elt newarr toix) (elt newarr fromix))
	       newarr))
	   (moveall (arr index)
	     (let* ((newlist nil)
		    (row (floor (/ index width)))
		    (col (mod index width)))
	       (if (can-move? (1- row) col) 
		   (push (move arr row col (1- row) col) newlist))
	       (if (can-move? row (1- col))
		   (push (move arr row col row (1- col)) newlist))
	       (if (can-move? (1+ row) col)
		   (push (move arr row col (1+ row) col) newlist))
	       (if (can-move? row (1+ col))
		   (push (move arr row col row (1+ col)) newlist))
	       newlist))
	   (findfrom (arr start)
	     (let ((pos (position #\. arr :start  start)))
	       (if (not pos) nil
		   (append (moveall arr pos) (findfrom arr (1+ pos)))))))
	(findfrom state 0))))

(deftest test-slide ()
  (check
   (equalp (solve (make-instance 'slide 
				 :start '((#\1 #\2 #\3)
					  (#\. #\. #\.))
				 :goal  '((#\. #\2 #\3)
					 (#\1 #\. #\.))))
	   '(#(#\1 #\2 #\3 #\. #\. #\.) #(#\. #\2 #\3 #\1 #\. #\.)))
   (equal (length (solve (make-instance 'slide 
				:start '((#\. #\. #\.)
					 (#\a #\b #\c)
					 (#\. #\. #\.))
				:goal  '((#\. #\. #\.)
					 (#\c #\b #\a)
					 (#\. #\. #\.)))))
	  9)
   (equal (length (solve (make-instance 'slide
				  :start '((#\. #\8 #\9)
					   (#\4 #\5 #\6))
				  :goal  '((#\6 #\5 #\4)
					   (#\. #\9 #\8)))))
	  14)))
