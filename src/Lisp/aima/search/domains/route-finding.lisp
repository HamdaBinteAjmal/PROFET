;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: search/domains/route-finding
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
;;;; Find a Route Between Cities on a Map

;;; This is a typical example of an enumerated problem, where the
;;; state space is defined explicitly as a graph rather than implicitly by
;;; a result function. Maps are not fully general problems, because
;;; there is at most one action to get between two states A and B and the
;;; action is named (Go A B); thus, for generating or writing out
;;; route-finding problems we use an intermediate data structure
;;; to represent the map more compactly.

(defstruct (route-finding-problem (:include enumerated-problem)
				  (:constructor create-route-finding-problem))
  "The problem of finding a route from one city to another on a map.
  By default it is a random map.  A state in a route-finding problem is just 
  the name of the current city. Note that a more complicated version of this
  problem would augment the state with considerations of time, gas
  used, wear on car, tolls to pay, etc."
  locations   ;;; a hash table of xy locations, indexed by city, for h-cost
  )

(defun make-route-finding-problem 
     (&key (n 0) 
	   (map (if (zerop n) (error "search/domains/route-finding: Need a number of cities or a map - make-route-finding-problem.") 
		  (random-route-map :n-cities n)))
	   (initial-state 'A) (goal-states (list 'B)))
  (create-route-finding-problem 
   :initial-state initial-state
   :goal-states goal-states
   :actions (map-actions map)
   :result (map-result map)
   :step-cost (map-step-cost map)
   :locations (map-locations map)))

(defmethod h-cost ((problem route-finding-problem) city-name)
  "The heuristic cost is the straight-line distance to the goal."
  (let ((locations (route-finding-problem-locations problem)))
    (xy-distance (gethash city-name locations)
		 (gethash (first (enumerated-problem-goal-states problem)) locations))))

(defun map-actions (map &aux (actions (make-hash-table :test #'equalp)))
  "Return a hash table of actions."
  (loop for city in map do
    (loop for (neighbor-name . distance) in (city-neighbors city) do (declare-ignore distance)
      (push `(Go ,(city-name city) ,neighbor-name)
	    (gethash (city-name city) actions))))
  actions)

(defun map-result (map &aux (result (make-hash-table :test #'equalp)))
  "Return a hash table of results."
  (loop for city in map do
    (loop for (neighbor-name . distance) in (city-neighbors city) do (declare-ignore distance)
      (setf (gethash (list (city-name city) 
			   `(Go ,(city-name city) ,neighbor-name))
		     result)
	    neighbor-name)))
  result)

(defun map-step-cost (map &aux (step-cost (make-hash-table :test #'equalp)))
  "Return a hash table of step-costs."
  (loop for city in map do
    (loop for (neighbor-name . distance) in (city-neighbors city) do
      (setf (gethash (list (city-name city) 
			   `(Go ,(city-name city) ,neighbor-name)
			   neighbor-name)
		     step-cost)
	    distance)))
  step-cost)

(defun map-locations (map &aux (locations (make-hash-table :test #'equalp)))
  "Return a hash table of locations."
  (loop for city in map do
    (setf (gethash (city-name city) locations) (city-loc city)))
  locations)


;;; A map is a list of cities. Each city is a data structure
;;; with type "list" so that it is easy to create explicit instances.

(defstruct (city (:type list))
  "A city's loc (location) is an #(x y) vector.  The neighbors slot holds
  a list of (city-name . distance-along-road) pairs.  Be careful to 
  distinguish between a city name and a city structure."
  name loc neighbors)

(defun random-route-map (&key (n-cities 10) (width 100) (height 100)
			      (min-roads 2)
			      (max-roads (min n-cities (+ min-roads 3))))
  "Return a random map with n-cities in it, and some roads between them.
  Each city is connected to between MIN-ROADS and MAX-ROADS other cities.
  The default is from 2 to 5.  The road between any two cities has a length 
  of 1 to 1.5 times the straight-line distance between them."
  ;; First build the cities
  (let ((map nil))
    (loop for i from 1 to n-cities do
	 (push (make-city :name (number->name i) :neighbors nil
			  :loc (@ (random width) (random height)))
	       map))
    ;; Now lay down the roads
    (loop for city in map do
	 (let* ((n-roads (max 0 (- (random-integer min-roads max-roads)
				   (length (city-neighbors city)))))
		(candidates
		 (sort (remove city (copy-list map)) #'<
		       :key #'(lambda (city2)
				(xy-distance (city-loc city)
					     (city-loc city2))))))
	   (loop while (and candidates (> n-roads 0)) do
		(let ((city2 (pop candidates)))
		  (when (and city2 (not (assoc (city-name city2)
					       (city-neighbors city))))
			(decf n-roads)
			(build-road city city2))))))
    map))

(defun build-road (city1 city2)
  "Construct a road between two cities."
  (let* ((road-distance (round (* (+ 1.0 (random 0.5)) 
				  (xy-distance (city-loc city1) (city-loc city2))))))
    (push (cons (city-name city1) road-distance) (city-neighbors city2))
    (push (cons (city-name city2) road-distance) (city-neighbors city1))))

(defun number->name (i)
  "Turn an integer into a symbol.  1-26 go to A-Z; beyond that use Ci"
  (if (<= 1 i 26)
      (aref '#(0 a b c d e f g h i j k l m n o p q r s t u v w x y z) i)
    (intern (format nil "C~D" i))))

(defun city-with-name (name map)
  "Return the city (data structure) with the given name."
  (find name map :key #'city-name))

(defun road-distance (city1 city2)
  "Return the road distance between two cities, which must be neighbors."
  (cdr (assoc (city-name city2) (city-neighbors city1))))
  
;;;; The Romanian Map

(defparameter *romania-map*
  '(
    (Arad       #( 91 492) ((Zerind . 75) (Sibiu . 140) (Timisoara . 118)))
    (Bucharest	#(400 327) ((Fagaras . 211) (Pitesti . 101) (Giurgiu . 90)
			   (Urziceni . 85)))
    (Craiova	#(253 288) ((Dobreta . 120) (RimnicuVilcea . 146) (Pitesti . 138)))
    (Dobreta	#(165 299) ((Mehadia . 75) (Craiova . 120)))
    (Eforie	#(562 293) ((Hirsova . 86)))
    (Fagaras	#(305 449) ((Sibiu . 99) (Bucharest . 211)))
    (Giurgiu	#(375 270) ((Bucharest . 90)))
    (Hirsova	#(534 350) ((Urziceni . 98) (Eforie . 86)))
    (Iasi	#(473 506) ((Neamt . 87) (Vaslui . 92)))
    (Lugoj	#(165 379) ((Timisoara . 111) (Mehadia . 70)))
    (Mehadia	#(168 339) ((Lugoj . 70) (Dobreta . 75)))
    (Neamt	#(406 537) ((Iasi . 87)))
    (Oradea	#(131 571) ((Zerind . 71) (Sibiu . 151)))
    (Pitesti	#(320 368) ((RimnicuVilcea . 97) (Craiova . 138) (Bucharest . 101)))
    (RimnicuVilcea	#(233 410) ((Sibiu . 80) (Pitesti . 97) (Craiova . 146)))
    (Sibiu	#(207 457) ((Arad . 140) (Oradea . 151) (Fagaras . 99)
			   (RimnicuVilcea . 80)))
    (Timisoara	#( 94 410) ((Arad . 118) (Lugoj . 111)))
    (Urziceni	#(456 350) ((Bucharest . 85) (Hirsova . 98) (Vaslui . 142)))
    (Vaslui	#(509 444) ((Iasi . 92) (Urziceni . 142)))
    (Zerind	#(108 531) ((Arad . 75) (Oradea . 71)))
    )
  "A representation of the map in Figure 3.2 [2e p 63].
  But note that the straight-line distances to Bucharest are NOT the 
  same as those in Fig. 4.1 [2e p 95].")

(defparameter *romania-problem* (make-route-finding-problem
					  :initial-state 'Arad
					  :goal-states (list 'Bucharest)
					  :map *romania-map*))


