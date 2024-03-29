;;;; Path Planning in 2 Dimensions with Convex Polygonal Obstacles
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
;;;; Defining the Vertex, Line, Polygon and Scene Types

(defstruct vertex
  xy           ;; the xy point for the vertex
  c-neighbor   ;; neighbour in clockwise direction
  a-neighbor   ;; neighbour in anti-clockwise direction
  visible      ;; list of vertices visible from here
  )

(defmethod print-object ((v vertex) stream)
  (format stream "#<V ~D,~D>" (xy-x (vertex-xy v)) (xy-y (vertex-xy v))))

(defstruct line
  xy1 xy2)

(defstruct polygon
  vertices n)

(defstruct scene
  polygons			; polygons comprising scene
  start				; vertex for start 
  goal				; vertex for goal
  )

;;;; Defining path planning problems

(defstruct (path-planning-problem (:include enumerated-goals-problem)
				     (:constructor create-path-planning-problem))
  "A problem involving moving among polygonal obstacles in 2D space.
  A state is the current vertex."
  scene)

(defun make-path-planning-problem (&key scene)
  "Define a constructor to build a problem, using the scene properly."
  (create-path-planning-problem 
   :scene scene
   :initial-state (scene-start scene)
   :goal-states (list (scene-goal scene))))

(defmethod successor-fn ((problem path-planning-problem) v1)
  "Return a list of (action . state) pairs, where the state is another
  vertex that is visible from the current vertex v1, and the action is a 
  delta (dx dy) from  v1 to the new one."
  (let ((p1 (vertex-xy v1)))
    (mapcar #'(lambda (v2) (let ((p2 (vertex-xy v2)))
			     (cons (@ (- (xy-x p2) (xy-x p1)) 
				      (- (xy-y p2) (xy-y p1)))
				   v2)))
	    (vertices-visible-from v1 (path-planning-problem-scene problem)))))

(defmethod step-cost ((problem path-planning-problem) vertex1 action vertex2)
  "The cost of an action is its distance."
  (declare-ignore vertex1 vertex2)
  (xy-length action))

(defmethod h-cost ((problem path-planning-problem) vertex)
  "The heuristic cost is the straight-line distance to the goal."
  (xy-distance (vertex-xy vertex) (vertex-xy (first (enumerated-problem-goal-states problem)))))

;;; Functions for testing whether one vertex is visible from another

(defun vertices-visible-from (v1 scene)
  "Find all the vertices that can be seen from this vertex."
  ;; When you find them, cache them under the vertex-visible slot.
  (or (vertex-visible v1)
      (setf (vertex-visible v1) (vertices-in-view v1 scene))))
	    
(defun vertices-in-view (v scene)
  "Find all the other vertices that can be seen from v."
  (delete v
   (let ((result nil))
    (loop for poly in (scene-polygons scene) do
	 (cond ((member v (polygon-vertices poly))
		(push (vertex-c-neighbor v) result)
		(push (vertex-a-neighbor v) result))
	       (t (loop for v2 in (polygon-vertices poly) do
		       (when (visible-p (vertex-xy v) (vertex-xy v2) scene)
			 (push v2 result))))))
    result)))

(defun visible-p (xy1 xy2 scene)
  "Predicate; return t iff xy1 is visible from xy2."
  (let ( (line (make-line :xy1 xy1 :xy2 xy2)) )
    (notany #'(lambda (poly) (line-intersects-poly? line poly))
	    (scene-polygons scene))))

(defun line-intersects-poly? (line poly)
  "Predicate; return t iff line intersects poly."
  (some #'(lambda (v1)
	    (let ((v2 (vertex-c-neighbor v1)))
	      (intersects line (make-line :xy1 (vertex-xy v1) :xy2 (vertex-xy v2)))))
	(polygon-vertices poly)))

(defun intersects (l1 l2)   
;;; l1 is line ab; l2 is line cd
;;; assume the lines cross at alpha a + (1-alpha) b, 
;;;     also known as beta c + (1-beta) d
;;; line segments intersect if 0<alpha,beta<1 unless they're parallel
  (let* ((a (line-xy1 l1))
	 (b (line-xy2 l1))
	 (c (line-xy1 l2))
	 (d (line-xy2 l2))
	 (xa (xy-x a)) (ya (xy-y a))
	 (xb (xy-x b)) (yb (xy-y b))
	 (xc (xy-x c)) (yc (xy-y c))
	 (xd (xy-x d)) (yd (xy-y d))
	 (q (- (* (- xa xb) (- yc yd))
	       (* (- ya yb) (- xc xd)))))
    (unless (= 0 q)
      (let ((alpha (/ (- (* (- xd xb) (- yc yd))
			 (* (- yd yb) (- xc xd)))
		      q))
	     (beta (/ (- (* (- xd xb) (- ya yb))
			   (* (- yd yb) (- xa xb)))
		         q)))
	(and (< 0 alpha 1) (< 0 beta 1))))))


;;;; Code for constructing the scene data structure

(defun create-scene (&key start goal polygons)
  "START and GOAL are xy points; polygons is a list of lists of vertices."
  (let ((polys (mapcar #'create-polygon 
		       (list* (list start) (list goal) polygons))))
    (make-scene :start (first (polygon-vertices (first polys)))
		:goal (first (polygon-vertices (second polys)))
		:polygons polys)))

(defun create-polygon (points)
  ;; Assumes that points are given in anticlockwise order (or in order, anyway)
  (let* ((vertices (mapcar #'(lambda (xy) (make-vertex :xy xy)) points))
	 (poly (make-polygon :vertices vertices)))
    (setf (polygon-n poly) (length vertices))
    (loop for v in vertices do
      (let ((v2 (or (second (member v vertices)) (first vertices))))
	(setf (vertex-a-neighbor v) v2)
	(setf (vertex-c-neighbor v2) v)))
    poly))      
    

(defparameter *Figure-4.17-scene*
  (create-scene 
   :start '(112 660) :goal '(353 573)
   :polygons
   '(((220 616) (220 666) (251 670) (272 647))
     ((341 655) (359 667) (374 651) (366 577))
     ((311 530) (311 559) (339 578) (361 560) (361 528) (336 516))
     ((105 628) (151 670) (180 629) (156 577) (113 587))
     ((118 517) (245 517) (245 557) (118 557))
     ((280 583) (333 583) (333 665) (280 665))
     ((252 594) (290 562) (264 538))
     ((198 635) (217 574) (182 574))
     ))
  "The scene in Figure 3.22 [2e p 92] with 8 obstacles.")

(defparameter *Figure-4.17-path-planning-problem*
  (make-path-planning-problem :scene *Figure-4.17-scene*))