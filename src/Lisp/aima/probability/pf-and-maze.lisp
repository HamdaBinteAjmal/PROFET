;;; a5-extra.lisp
;;; 
 ;; PROFET Copyright 2015 (c) Data Mining and Machine Learning Group,
 ;; National University of Ireland Galway.  
 ;; This file is a part of AIMA Framework
 ;;
 ;; Licensed under the Apache License, Version 2.0 (the "License");
 ;; you may not use this file except in compliance with the License.
 ;; You may obtain a copy of the License at
 ;;
 ;;      http://www.apache.org/licenses/LICENSE-2.0
 ;;
 ;; Unless required by applicable law or agreed to in writing, software
 ;; distributed under the License is distributed on an "AS IS" BASIS,
 ;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 ;; See the License for the specific language governing permissions and
 ;; limitations under the License.
;;;
;;; Contains code for grid mazes (including partially complete code for converting
;;; a maze to a DBN model) and partially complete code for particle filtering.


;;;; Definitions for grid mazes including two examples.

;;; A maze is an array in which each element is a list of the
;;; directions in which movement is *possible*. You can get a better
;;; idea of what a maze looks like by doing, e.g.,
;;; (print-maze-posterior nil *maze10x10*) and ignoring the nils.

;;; Given a maze, we can define a DBN that describes movement and
;;; perception in that maze. The state variable is the agent's
;;; position (an xy pair).  The agent's actions are observable and
;;; chosen uniformly at random from (up left down right). The
;;; "geography" of the maze is implicit in the CPT for position_{t+1}
;;; given position_t and action_t.  Movement *occurs* ;;; according to
;;; the standard stochastic model [2e p 614]: with probability (1-
;;; p_fail) motion occurs in the intended direction, and with
;;; probability p_fail/2 motion occurs in each of the perpendicular
;;; directions; but if the actual motion goes into a wall, the agent
;;; stays put. (On p614, then, p_fail is 0.2.)  The agent's perception
;;; is very limited, consisting only of the *number* of detected
;;; adjacent walls, where each actual adjacent wall is missed
;;; with probability p_miss. 


(defvar *maze-actions*)
(setq *maze-actions* '(up down left right))

(defvar *maze-percepts*)
(setq *maze-percepts* '(0 1 2 3 4))

(defvar *maze2x2*)
(setq *maze2x2* #2A(((UP RIGHT) (DOWN)) ((UP LEFT) (DOWN))))

(defvar *maze2x2-e10*)
(setq *maze2x2-e10* #(0 NIL 2 2 NIL 3 3 NIL 3 3 NIL 2 1 NIL 3 2 NIL 2 2 NIL 1 3 NIL 2 1 NIL 1 3 NIL 2 3 NIL 2))

(defvar *maze10x10*)
(setq *maze10x10* #2A(((UP RIGHT) (RIGHT UP DOWN) (DOWN) (RIGHT) (UP) (UP DOWN RIGHT)
     (UP DOWN) (RIGHT UP DOWN) (DOWN) (RIGHT))
    ((LEFT) (UP RIGHT LEFT) (UP RIGHT DOWN) (RIGHT UP LEFT DOWN) (DOWN)
     (LEFT RIGHT) (RIGHT) (UP LEFT) (RIGHT UP DOWN) (LEFT DOWN))
    ((RIGHT) (RIGHT LEFT) (RIGHT LEFT) (RIGHT UP LEFT) (UP DOWN)
     (DOWN LEFT UP) (DOWN LEFT UP RIGHT) (DOWN) (UP RIGHT LEFT)
     (RIGHT DOWN))
    ((RIGHT LEFT UP) (LEFT RIGHT DOWN UP) (LEFT DOWN RIGHT UP)
     (DOWN RIGHT UP LEFT) (DOWN) (UP) (LEFT DOWN UP RIGHT) (DOWN)
     (LEFT) (RIGHT LEFT))
    ((LEFT RIGHT UP) (DOWN RIGHT LEFT) (LEFT) (LEFT) (RIGHT UP)
     (RIGHT DOWN UP) (LEFT UP DOWN RIGHT) (UP DOWN) (DOWN RIGHT UP)
     (DOWN RIGHT LEFT))
    ((RIGHT LEFT) (LEFT) (UP) (RIGHT DOWN UP) (UP RIGHT DOWN LEFT)
     (DOWN LEFT UP) (LEFT UP DOWN RIGHT) (DOWN) (RIGHT LEFT) (LEFT))
    ((UP RIGHT LEFT) (RIGHT UP DOWN) (DOWN) (LEFT UP) (RIGHT DOWN LEFT)
     (UP) (LEFT UP DOWN RIGHT) (RIGHT DOWN) (UP RIGHT LEFT)
     (RIGHT DOWN))
    ((UP LEFT) (DOWN UP RIGHT LEFT) (RIGHT UP DOWN) (DOWN) (LEFT)
     (RIGHT) (LEFT UP RIGHT) (RIGHT LEFT DOWN) (LEFT) (RIGHT LEFT))
    ((RIGHT) (LEFT) (RIGHT UP LEFT) (DOWN) (RIGHT) (UP LEFT RIGHT)
     (LEFT UP DOWN) (LEFT DOWN) (RIGHT UP) (DOWN RIGHT LEFT))
    ((LEFT UP) (DOWN UP) (UP DOWN LEFT) (UP DOWN) (UP LEFT DOWN)
     (LEFT UP DOWN) (DOWN) (UP) (DOWN LEFT) (LEFT))))



(defun maze->dbn (maze &key (p_miss 0.1) (p_fail 0.2))
  "Returns a DBN with position, action, and percept nodes for movement in a maze."
  (let* ((xsize (array-dimension maze 0))
	 (ysize (array-dimension maze 1))
	 (action_0 (make-tabulated-bnode :name 'action_0 :arity (length *maze-actions*)
					 :index 0 :value-names *maze-actions*
					 :parents nil))
	 (action_1 (make-tabulated-bnode :name 'action_1 :arity (length *maze-actions*)
					 :index 3 :value-names '(up down left right)
					 :parents nil))
	 (position_0 (make-tabulated-bnode :name 'position_0 :arity (* xsize ysize) :index 1
					   :value-names (mapcan #'(lambda (x) 
							       (mapcar #'(lambda (y) (list x y)) (iota ysize)))
							   (iota xsize))
					   :parents nil))
	 (position_1 (make-tabulated-bnode :name 'position_1 :arity (* xsize ysize) :index 4
					   :value-names (mapcan #'(lambda (x) 
							       (mapcar #'(lambda (y) (list x y)) (iota ysize)))
							   (iota xsize))
					   :parents (list position_0 action_0)))
	 (percept_0 (make-tabulated-bnode :name 'percept_0 :arity (length *maze-percepts*) 
					  :index 2 :value-names *maze-percepts*
					  :parents nil))
	 (percept_1 (make-tabulated-bnode :name 'percept_1 :arity (length *maze-percepts*)
					  :index 5 :value-names *maze-percepts*
					  :parents (list position_1))))
    ;; Define the children for each node
    (setf (bnode-children action_0) (list position_1))
    (setf (bnode-children action_1) nil)
    (setf (bnode-children position_0) (list position_1))
    (setf (bnode-children position_1) (list percept_1))
    (setf (bnode-children percept_0) nil)
    (setf (bnode-children percept_1) nil)
    ;; Fill in the CPTs for the action nodes - uniform random selection
    (setf (tabulated-bnode-cpt action_0) 
	  (make-array nil :initial-element (make-distribution (discrete-bnode-arity action_0))))
    (setf (tabulated-bnode-cpt action_1) 
	  (make-array nil :initial-element (make-distribution (discrete-bnode-arity action_1))))
    ;; Fill in CPTs for percept node. Percept_0 is irrelevant.
    (setf (tabulated-bnode-cpt percept_0) 
	  (make-array nil :initial-element (make-distribution (discrete-bnode-arity percept_0))))
    (setf (tabulated-bnode-cpt percept_1) (make-array (mapcar #'discrete-bnode-arity 
							      (bnode-parents percept_1))))
    (loop for xy in (discrete-bnode-value-names position_1) 
	  and i from 0 to (1- (discrete-bnode-arity position_1)) do
      (setf (aref (tabulated-bnode-cpt percept_1) i) 
	    (make-maze-percept-distribution xy maze p_miss)))
    ;; Fill in CPTs for position nodes. Position_0 is chosen uniformly at random.
    (setf (tabulated-bnode-cpt position_0) 
	  (make-array nil :initial-element (make-distribution (discrete-bnode-arity position_0))))
    (setf (tabulated-bnode-cpt position_1) 
	  (make-array (mapcar #'discrete-bnode-arity (bnode-parents position_1))))
    ;; Fill in a distribution for position_1 given each possible action_0 and position_0
    (loop for xy in (discrete-bnode-value-names position_0) 
	  and i from 0 to (1- (discrete-bnode-arity position_0)) do
      (loop for a in (discrete-bnode-value-names action_0) 
	    and j from 0 to (1- (discrete-bnode-arity action_0)) do
	(setf (aref (tabulated-bnode-cpt position_1) i j)
	      (make-maze-position-distribution 
	       xy a maze p_fail (discrete-bnode-value-names position_1)))))
    ;; Return the DBN with slices defined appropriately.
    (make-dbn :var-names '(action position percept)
	      :slice0 (list action_0 position_0 percept_0)
	      :slice1 (list action_1 position_1 percept_1)
	      :nodes (list action_0 position_0 percept_0
			   action_1 position_1 percept_1))))

(defun make-maze-percept-distribution (xy maze p_miss)
  "Return a distribution over the number of perceived walls at position xy;
   the true number number of walls can be extracted from the maze array
   and the probability of missing each is p_miss."
  ;; >>> WRITE THIS PART
  )

(defun make-maze-position-distribution (xy a maze p_fail squares)
  "Return a distribution giving, for each square in squares, the
   probability that action a in square xy reaches that square."
  ;; >>> WRITE THIS PART
  )

(defun xy-reached (xy a possible-actions)
  "Return the square reached by a move actually taken 
   (either the square ahead if the move is possible, or the same square if not)."
  (if (member a possible-actions)
      (case a
       (up (@ (xy-x xy) (1+ (xy-y xy))))
       (left (@ (1- (xy-x xy)) (xy-y xy)))
       (down (@ (xy-x xy) (1- (xy-y xy))))
       (right (@ (1+ (xy-x xy)) (xy-y xy))))
    xy))

(defun clockwise90 (a) 
  "Return the direction 90 degrees clockwise from direction a."
  (case a (up 'right) (left 'up) (down 'left) (right 'down)))

(defun anticlockwise90 (a) 
  "Return the direction 90 degrees anticlockwise from direction a."
  (case a (up 'left) (left 'down) (down 'right) (right 'up)))

(defun print-maze-posterior (posterior maze)
  (let ((xsize (array-dimension maze 0))
	(ysize (array-dimension maze 1)))
    (loop for y from (1- ysize) downto 0 do
	  (format t "~%")
	  (loop for x from 0 to (1- xsize) do
		(format t "*")
		(format t (if (member 'up (aref maze x y)) "        " "--------")))
	  (format t "*~%")
	  (loop for x from 0 to (1- xsize) do
		(format t (if (member 'left (aref maze x y)) " " "|"))
		(format t "        "))
	  (format t "|~%")
	  (loop for x from 0 to (1- xsize) do
		(format t (if (member 'left (aref maze x y)) " " "|"))
		(format t " ~6,4F " (cdr (assoc (@ x y) posterior :test #'equal))))
	  (format t "|~%")
	  (loop for x from 0 to (1- xsize) do
		(format t (if (member 'left (aref maze x y)) " " "|"))
		(format t "        "))
	  (format t "|"))
    (format t "~%")
    (loop for x from 0 to (1- xsize) do
	  (format t "*--------"))
    (format t "*~%")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun particle-filter-by-name (Xname e_0-to-k dbn &optional (N 1000))
  "Returns a list of posterior distributions for unsubscripted Xname given e_0-to-i, 
   for time steps i=0...k where k is determined from the length of e_0-to-k."
  (let* ((X_0 (bnode-by-name (subscript Xname 0) dbn))   ; Query variable in slice 0
	 (X_1 (bnode-by-name (subscript Xname 1) dbn))   ; Query variable in slice 1
	 (Ncounts (make-distribution                     ; Query value counts for current time step
		   (discrete-bnode-arity X_0) 0.0d0))
	 (slice-size (length (dbn-var-names dbn)))       ; Number of variables per slice
	 (k (1- (/ (length e_0-to-k) slice-size)))       ; Total number of slices in evidence (not including slice 0)
	 (start 0)                                       ; Position in evidence vector where current step starts
	 (particles (make-sequence 'vector N))           ; Current set of particles (an N-element vector)
	 (resampled-particles (make-sequence 'vector N)) ; Receptacle for particles resampled from original set
	 (weights (make-sequence 'vector N))             ; Weight for each particle given current evidence
	 (results nil))                                  ; List of filtered distributions per time step

    (loop for j from 0 to (1- N) do
      (setf (aref particles j) (make-sequence 'vector (* 2 slice-size)))
      (setf (aref resampled-particles j) (make-sequence 'vector (* 2 slice-size))))

    ;; Each particle is a vector of values for the variables in slices 0 and 1.
    ;; Initialize the particles with values for slice 0
    (zero-distribution Ncounts)
    (loop for j from 0 to (1- N) do
      (let ((particle (aref particles j)))
        ;; Copy the evidence into slice 0
	(loop for index from 0 to (1- slice-size) do
	  (setf (aref particle index) (aref e_0-to-k (+ start index))))
	;; Sample the non-evidence variables and save the particle weight
	(setf (aref weights j) (weighted-sample-slice particle 0 slice-size dbn))
	(increment-distribution Ncounts (node-value X_0 particle) (aref weights j))))
    (push (distribution->alist (dnormalize Ncounts) X_1) results)
    (weighted-resample N particles (dnormalize weights) resampled-particles)
    (rotatef particles resampled-particles)
    (incf start slice-size)

    (loop for i from 1 to k do
      (zero-distribution Ncounts)
      (loop for j from 0 to (1- N) do
	(let ((particle (aref particles j)))
	  (when (> i 1) 
	    ;; move the particle state and evidence back from slice 1 to slice 0
	    (loop for index from 0 to (1- slice-size) do
	      (setf (aref particle index) (aref particle (+ index slice-size)))))

	  ;; assert new evidence in slice 1
	  (loop for index from 0 to (1- slice-size) do
	    (setf (aref particle (+ slice-size index)) (aref e_0-to-k (+ start index))))

	  ;; Propagate slice 0 to slice 1 and collect evidence weight
	  (setf (aref weights j) (weighted-sample-slice particle slice-size slice-size dbn))
	  (increment-distribution Ncounts (node-value X_1 particle) (aref weights j))))

      (push (distribution->alist (dnormalize Ncounts t) X_1) results)
      (weighted-resample N particles (dnormalize weights t) resampled-particles)
      (rotatef particles resampled-particles)
      (incf start slice-size)
      )
    (reverse results)))

(defun weighted-sample-slice (particle start-index slice-size dbn &aux x_i-value)
  "Samples non-evidence variables one slice of the particle, starting at the position
   indicated by start-index, and computes the particle weight as the product
   of the conditional probabilities for the evidence variables in the slice
   (just like likelihood weighting, except that no copy needs to be made).
   Returns the particle weight."
  ;; >>> WRITE THIS PART
  )

(defun weighted-resample (N particles weights resampled-particles)
  "Modifies and returns the resampled-particles vector to contain
   N samples from the weighted particles vector. Note that we must be
   careful to copy the *contents* of the particle to avoid sharing structure."
  ;; >>> WRITE THIS PART
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-filter (approx-by-name Xname e dbn Ns num-trials file-prefix)
  "Uses the approx-by-name filtering algorithm to filer on Xname given e in dbn.
   On each run, the filtered distributions are compared to the exact answers
   computed by unrolling, and the RMS error is calculated at each time step.
   For each number of samples N in Ns, does the complete filtering run
   num-trials-times and averages the results, then writes them to
   a file whose name is file-prefixN.data"
  (let ((k (1- (/ (length e) (length (dbn-var-names dbn)))))
	(exact (unroll-filter-by-name 'position e dbn)))
    (loop for N in Ns do
      (plot-alist (mapcar #'cons (iota (1+ k)) 
			  (let ((totals (make-list (1+ k) :initial-element 0.0d0)))
			    (loop for trial from 1 to num-trials do
			      (setf totals (mapcar #'+ totals
						   (mapcar #'rms-error-enumerated 
							   exact 
							   (funcall approx-by-name Xname e dbn N)))))
			    (mapcar #'(lambda (x) (/ x (1+ k))) totals)))
		  (concatenate 'string file-prefix (format nil "~A" N) ".data")))))

