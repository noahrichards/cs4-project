(in-package :com.noahsmark.cs4project)

(defclass water ()
  ((goal
    :initarg :goal
    :initform 4
    :reader goal
    :documentation "Goal bucket amount.")
   (buckets
    :initarg :buckets
    :initform '(3 5)
    :reader buckets
    :documentation "Sizes of buckets to use in solving the puzzle.")))

(defmethod init-state ((water water))
  (make-list (length (buckets water)) :initial-element 0))

(defmethod is-end? ((water water) state)
  (labels
      ((checkval (bucket)
	 (cond
	   ((endp bucket) nil)
	   ((= (car bucket) (goal water)) t)
	   (t (checkval (cdr bucket))))))
    (checkval state)))

(defmethod get-neighbors ((water water) state)
  (let ((buckets (buckets water)))
    (labels
	(
	 ;; Dump one bucket at a time
	 (dump-loop (&optional (n 0))
	   (if (= n (length buckets)) nil
	       (cons 
		(nsubstitute-if 0 #'atom (copy-list state) :start n :end (1+ n)) 
		(dump-loop (1+ n)))))
	 ;; Fill one bucket at a time to maximum capacity
	 (fill-loop (&optional (n 0))
	   (if (= n (length buckets)) nil
	       (cons 
		(nsubstitute-if (elt buckets n) #'atom 
				(copy-list state) :start n :end (1+ n)) 
		(fill-loop (1+ n)))))
	 ;; Move from each bucket to each other bucket
	 (move-loop (&optional (i 0) (j 0))
	   (cond
	     ((= i j) (move-loop i (1+ j)))
	     ((>= j (length buckets)) (move-loop (1+ i) 0))
	     ((>= i (length buckets)) nil)
	     (t (cons (move-from-to i j) (move-loop i (1+ j))))))
	 ;; Helper for move-loop (TODO: Make this into a macro)
	 (move-from-to (i j)
	   (let ((moveamt
		  (if (<= (- (elt buckets j) (elt state j))
			  (elt state i)) (- (elt buckets j) (elt state j))
			  (elt state i))))
	     (nsubstitute-if (- (elt state i) moveamt)
			     #'atom
			     (nsubstitute-if (+ (elt state j) moveamt)
					     #'atom
					     (copy-list state)
					     :start j
					     :end (1+ j))
			     :start i
			     :end (1+ i)))))
      (append (dump-loop) (fill-loop) (move-loop)))))


(deftest test-water ()
  (check
   (equalp (solve (make-instance 'water :goal 2 :buckets '(2)))
	   '((0) (2)))                     ;easy fill
   (equalp (solve (make-instance 'water :goal 2 :buckets '(3 5)))
	   '((0 0) (0 5) (3 2)))           ;fill and move
   (equalp (solve (make-instance 'water :goal 6 :buckets '(2 10)))
	   '((0 0) (0 10) (2 8) (0 8)      ;fill, move, and dump (simple)
	     (2 6)))
   (equalp (solve (make-instance 'water :goal 4 :buckets '(3 5)))
	   '((0 0) (0 5) (3 2) (0 2)       ;fill, move, and dump (hard)
	     (2 0) (2 5) (3 4)))
   (eq     (solve (make-instance 'water :goal 13 :buckets '(10 1 1 1)))
	   nil)))                          ;unsolvable (no bucket large enough)
