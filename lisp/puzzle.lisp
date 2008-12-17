(in-package :com.noahsmark.cs4project)

(defgeneric init-state (puzzle)
  (:documentation "Get the starting state for the puzzle."))

(defgeneric is-end? (puzzle state)
  (:documentation "Check and see if the given state (for this puzzle) is
a 'solved' state."))

(defgeneric get-neighbors (puzzle state)
  (:documentation "Get a list of all the neighbors of this state."))