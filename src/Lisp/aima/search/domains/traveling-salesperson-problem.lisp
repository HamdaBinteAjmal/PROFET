;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: tsp.lisp
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
;;;; The Travelling Salesperson Problem (TSP)

;;; Find a tour: a path that visits every city exactly once, and returns to
;;; the starting city.  The shorter the total distance, the better.  This
;;; builds on the map data structure defined in route-finding.lisp.  It
;;; assumes that the map is a complete graph: there is a path from every city
;;; to every other city.
;;;
;;; Note: the TSP is NP complete in the general case, but there are some good
;;; algorithms for finding approximate solutions, particularly when the
;;; triangle inequality is satisfied (that the path from A->C is always
;;; shorter than A->B->C).  Many of these algorithms are based on the idea of
;;; building a minimum spanning tree, converting it into a tour, and perhaps
;;; modifying it.  We don't go into that here (because we are more interested
;;; in hooking up to the general search procedures than in special-purpose
;;; algorithms), but note that our tsp-h heuristic function is a relaxed
;;; version of a minimum spanning tree.

(defstruct (tsp (:include problem) (:constructor create-tsp))
  (map nil))

(defstruct tsp-state
  "A state for a TSP problem lists cities visited and cities as yet unvisited."
  (visited nil)			; List of names of cities visited so far
  (to-visit nil)		; Set of names of cities left to visit
  )

(defmethod copy-state ((state tsp-state))
  "Return a new state with the same cities in fresh lists."
  (make-tsp-state :visited (copy-list (tsp-state-visited state))
		  :to-visit (copy-list (tsp-state-to-visit state))))

(defun make-tsp (&key (n 0) 
		      (map (if (zerop n) (error "search/domains/traveling-salesperson-problem: Need a number of cities or a map - make-tsp.") 
			     (random-tsp-map :n-cities n)))
		      (name0 (city-name (first map))))
  "Constructor for TSP problems.  The map must be a complete graph."
  (check-tsp-map? map)
  (create-tsp 
   :initial-state (make-tsp-state :visited (list name0)
				  :to-visit (remove name0 (mapcar #'city-name map)))
   :map map))

(defmethod step-cost ((problem tsp) state1 action state2)
  (declare (ignore action))
  (road-distance (city-with-name (current-city-name state1) (tsp-map problem))
		 (city-with-name (current-city-name state2) (tsp-map problem))))

(defmethod h-cost ((problem tsp) state)
  "A lower bound on the completion cost from here is the sum of
     the shortest distance from here to the remaining cities
   + the shortest distance from the remaining cities back to the start
   + a lower bound on the cost to traverse the remaining cities.
   This is not as good as Minimum Spanning Tree, but is much simpler."
  (let ((to-visit (tsp-state-to-visit state))
	(map (tsp-map problem)))
    (+ (nearest-neighbor-distance (current-city-name state) to-visit map)
       (nearest-neighbor-distance (starting-city-name state) to-visit map)
       (path-lower-bound to-visit map))))

(defmethod actions ((problem tsp) state)
  "You can only go to a city you haven't visited yet,
   unless you've visited them all, in which case you can only go back home."
  (let ((name1 (current-city-name state))
	(name0 (starting-city-name state))
	(actions nil))
    (if (null (tsp-state-to-visit state))
	(setf actions (list `(Go ,name1 ,name0)))
      (let ((city1 (city-with-name name1 (tsp-map problem))))
        (loop for (name2 . distance) in (city-neighbors city1) do (declare-ignore distance)
	  (when (member name2 (tsp-state-to-visit state))
	    (push `(Go ,name1 ,name2) actions)))))
    actions))

(defmethod result ((problem tsp) action state)
  (let ((next-city-name (third action)))
    (make-tsp-state :visited (cons next-city-name (tsp-state-visited state))
		    :to-visit (remove next-city-name (tsp-state-to-visit state)))))


(defmethod goal-test ((problem tsp) state)
  "The goal is to leave no unvisited cities and get back to start."
  (and (null (tsp-state-to-visit state))
       (eql (current-city-name state) 
	    (current-city-name (problem-initial-state problem)))))

;;;; Auxiliary Functions

(defun nearest-neighbor-distance (name candidate-names map)
  "Find among the CANDIDATE-NAMES of cities, the one that is closest to
  city NAME, and return the distance to it."
  (if (null candidate-names)
      0
    (let ((city (city-with-name name map))
	  (distance infinity))
      (loop for other-name in candidate-names do
	(unless (eq other-name name)
	  (setf distance (min distance (road-distance city (city-with-name other-name map))))))
      distance)))

(defun path-lower-bound (city-names map)
  "Find a lower bound for a path through these cities."
  ;; Each city must be connected to a next one, for n-1 links for n cities.
  ;; A lower bound is the sum of the shortest links for each city but first.
  (let ((sum 0))
   (loop for name in (rest city-names) do
	(incf sum (nearest-neighbor-distance name city-names map)))
   sum))

(defun random-tsp-map (&key (n-cities 6))
  (random-route-map :n-cities n-cities :min-roads (- n-cities 1)
			       :max-roads (- n-cities 1)))

(defun check-tsp-map? (map)
  (loop for city in map do
       (when (/= (length (city-neighbors city)) (- (length map) 1))
	 (error "search/domains/traveling-salesperson-problem: This map can't be used for a travelling salesperson problem ~
                because ~A is not connected to every other city - check-tsp-map."
		(city-name city)))))

(defmethod current-city-name ((state tsp-state))
  "The current city: the last one visited."
  ;; We store the cities visited in reverse order, so take the first one
  (first (tsp-state-visited state)))

(defmethod starting-city-name ((state tsp-state))
  (last1 (tsp-state-visited state)))

