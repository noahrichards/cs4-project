(in-package :com.noahsmark.cs4project)


;;; Main solver method
(defun solve (puzzle)
  "Solve a given puzzle, which contains the following
 1) A function for obtaining the initial state
 2) A function for obtaining neighbors of a given state
 3) A function to test for a solved state

 Puzzles themselves are stateless, and neighbors should be
 determined based only on the given state.

 The solver uses a queue to simulate a breadth-first search of the
 tree-expansion of the puzzle"
  (let ((init-state (init-state puzzle))
        (memo (make-hash-table :test #'equalp)))
    (labels (
             ;; Make an answer, assuming the passed in list
             ;; is (endstate)
             (makeanswer (answer)
                 (if (equalp (car answer) init-state)
                   answer
                   (makeanswer (cons
                                 (gethash (car answer) memo)
                                 answer))))
             ;; Prune out the states in the list that
             ;; have already been visited (by checking the
             ;; hash memo)
             (prune (state lyst)
                    (cond
                      ((null lyst) nil)
                      ;; Already seen
                      ((gethash (car lyst) memo)
                       (prune state (cdr lyst)))
                      ;; Not seen, add to map (with value being last state)
                      (t
                        (setf (gethash (car lyst) memo) state)
                        ;(format t "Adding new state: ~a~%" (car lyst))
                        (cons (car lyst) (prune state (cdr lyst))))))
             ;; Solve the puzzle, using an accumulator as a
             ;; queue
             (solver (acc)
                 (cond 
                   ((null acc) nil)
                   ;; If this is the solution state, we are done
                   ((is-end? puzzle (car acc)) 
                    (makeanswer (list (car acc))))
                   ;; Else, append the neighbors that haven't been seen yet
                   ;; to the end of the queue of states to visit
                   (t (solver 
                        (append (cdr acc) 
                                (prune (car acc) (get-neighbors puzzle (car acc)))))))))
      ; Initialize the hash to include the start state
      (setf (gethash init-state memo) t)
      (solver (list init-state)))))
