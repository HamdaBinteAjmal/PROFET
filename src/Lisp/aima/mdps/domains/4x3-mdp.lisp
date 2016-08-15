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
;;;; Stochastic active 4x3 world for chapters 17, 20.

;;; Each action achieves the intended effect with probability 0.8, but the
;;; rest of the time, the action moves the agent at right angles to the
;;; intended direction.  For example, from the start square (1,1), the
;;; action North moves the agent to (1,2) with probability 0.8, but with
;;; probability 0.1, it moves East to (2,1), and with probability 0.1,
;;; it moves West, bumps into the wall, and stays in (1,1).

(defvar *4x3-mdp*)

(setq *4x3-mdp*
  (make-enumerated-mdp
   :initial-state '(1 1)
   :terminal-states '((4 2) (4 3))
   :states '((1 1) (1 2) (1 3) (2 1) (2 3) (3 1) (3 2) (3 3) (4 1) (4 2) (4 3))
   :reward-type 's
   :reward (alist->hash-table
	     '(((1 1) . -0.04)
	       ((1 2) . -0.04)
	       ((1 3) . -0.04)
	       ((2 1) . -0.04)
	       ((2 3) . -0.04)
	       ((3 1) . -0.04)
	       ((3 2) . -0.04)
	       ((3 3) . -0.04)
	       ((4 1) . -0.04)
	       ((4 2) . -1.00)
	       ((4 3) . +1.00)))
   :actions (alist->hash-table
	     '(((1 1) . (left right up down))
	       ((1 2) . (left right up down))
	       ((1 3) . (left right up down))
	       ((2 1) . (left right up down))
	       ((2 3) . (left right up down))
	       ((3 1) . (left right up down))
	       ((3 2) . (left right up down))
	       ((3 3) . (left right up down))
	       ((4 1) . (left right up down))
	       ((4 2) . nil)
	       ((4 3) . nil)))
   :results (alist->hash-table
	     `((((1 1) left) . (((1 1) . 0.9) ((1 2) . 0.1)))
	       (((1 1) right) . (((2 1) . 0.8) ((1 2) . 0.1) ((1 1) . 0.1)))
	       (((1 1) up) . (((1 2) . 0.8) ((2 1) . 0.1) ((1 1) . 0.1)))
	       (((1 1) down) . (((1 1) . 0.9)  ((2 1) . 0.1)))
	       (((1 2) left) . (((1 2) . 0.8) ((1 1) . 0.1) ((1 3) . 0.1)))
	       (((1 2) right) . (((1 2) . 0.8) ((1 1) . 0.1) ((1 3) . 0.1)))
	       (((1 2) up) . (((1 2) . 0.2)  ((1 3) . 0.8)))
	       (((1 2) down) . (((1 2) . 0.2) ((1 1) . 0.8)))
	       (((1 3) left) . (((1 3) . 0.9)  ((1 2) . 0.1)))
	       (((1 3) right) . (((1 3) . 0.1) ((2 3) . 0.8) ((1 2) . 0.1))) 
	       (((1 3) up) . (((1 3) . 0.9) ((2 3) . 0.1))) 
	       (((1 3) down) . (((1 3) . 0.1) ((2 3) . 0.1) ((1 2) . 0.8)))
	       (((2 1) left) . (((2 1) . 0.2) ((1 1) . 0.8)))
	       (((2 1) right) . (((2 1) . 0.2)  ((3 1) . 0.8)))
	       (((2 1) up) . (((2 1) . 0.8) ((1 1) . 0.1) ((3 1) . 0.1)))
	       (((2 1) down) . (((2 1) . 0.8) ((1 1) . 0.1) ((3 1) . 0.1)))
	       (((2 3) left) . (((2 3) . 0.2) ((1 3) . 0.8))) 
	       (((2 3) right) . (((2 3) . 0.2)   ((3 3)  . 0.8))) 
	       (((2 3) up) . (((2 3) . 0.8) ((1 3) . 0.1)  ((3 3)  . 0.1))) 
	       (((2 3) down) . (((2 3) . 0.8) ((1 3) . 0.1)  ((3 3)  . 0.1)))
	       (((3 1) left) . (((3 1) . 0.1) ((3 2) . 0.1) ((2 1) . 0.8))) 
	       (((3 1) right) . (((3 1) . 0.1) ((3 2) . 0.1) ((4 1) . 0.8))) 
	       (((3 1) up) . (((3 2) . 0.8) ((2 1) . 0.1) ((4 1) . 0.1))) 
	       (((3 1) down) . (((3 1) . 0.8)  ((2 1) . 0.1) ((4 1) . 0.1)))
	       (((3 2) left) . (((3 2) . 0.8) ((3 1) . 0.1)  ((3 3)  . 0.1))) 
	       (((3 2) right) . (((4 2) . 0.8) ((3 1) . 0.1)  ((3 3)  . 0.1))) 
	       (((3 2) up) . (((3 2) . 0.1) ((4 2) . 0.1)  ((3 3)  . 0.8))) 
	       (((3 2) down) . (((3 2) . 0.1) ((4 2) . 0.1)  ((3 1)  . 0.8)))
	       (((3 3) left) . (((2 3) . 0.8) ((3 3) . 0.1)  ((3 2)  . 0.1))) 
	       (((3 3) right) . (((3 2) . 0.1) ((4 3) . 0.8)  ((3 3)  . 0.1))) 
	       (((3 3) up) . (((2 3) . 0.1) ((4 3) . 0.1)  ((3 3)  . 0.8))) 
	       (((3 3) down) . (((3 2) . 0.8) ((2 3) . 0.1)  ((4 3)  . 0.1)))
	       (((4 1) left) . (((4 1) . 0.1) ((3 1) . 0.8)  ((4 2)  . 0.1))) 
	       (((4 1) right) . (((4 1) . 0.9) ((4 2) . 0.1))) 
	       (((4 1) up) . (((4 2) . 0.8) ((4 1) . 0.1)  ((3 2)  . 0.1))) 
	       (((4 1) down) . (((4 1) . 0.9) ((3 1) . 0.1)))
	       (((4 2) left) . (((4 2) . 1.0)))
	       (((4 2) right) . (((4 2) . 1.0)))
	       (((4 2) up) . (((4 2) . 1.0)))
	       (((4 2) down) . (((4 2) . 1.0)))
	       (((4 3) left) . (((4 3) . 1.0)))
	       (((4 3) right) . (((4 3) . 1.0)))
	       (((4 3) up) . (((4 3) . 1.0)))
	       (((4 3) down) . (((4 3) . 1.0)))
	       ))))



