;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: search/test.lisp
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
;;;; Test Cases for Search

(deftest search
  "Test the code for Solving Problems by Searching"

  "First we test all the sequential search algorithms on all
   sequential problems. We check that the solution returned 
   by the algorithm actually reaches a goal."

  ((setq p1 (make-vacuum-problem :dirt-probability 1.0)))
  ((goal-test p1 (sequence-result p1 (breadth-first-tree-search p1) (problem-initial-state p1))) *)
  ((goal-test p1 (sequence-result p1 (uniform-cost-tree-search p1) (problem-initial-state p1))) *)
  ;;; depth-first-tree-search goes into an infinite loop
  ((goal-test p1 (sequence-result p1 (iterative-deepening-search p1) (problem-initial-state p1))) *)
  ((goal-test p1 (sequence-result p1 (greedy-best-first-tree-search p1) (problem-initial-state p1))) *)
  ((goal-test p1 (sequence-result p1 (a*-tree-search p1) (problem-initial-state p1))) *)
  ((goal-test p1 (sequence-result p1 (ida* p1) (problem-initial-state p1))) *)
  ((goal-test p1 (sequence-result p1 (recursive-best-first-search p1) (problem-initial-state p1))) *)
  ((goal-test p1 (sequence-result p1 (breadth-first-graph-search p1) (problem-initial-state p1))) *)
  ((goal-test p1 (sequence-result p1 (uniform-cost-graph-search p1) (problem-initial-state p1))) *)
  ((goal-test p1 (sequence-result p1 (depth-first-graph-search p1) (problem-initial-state p1))) *)
  ((goal-test p1 (sequence-result p1 (greedy-best-first-graph-search p1) (problem-initial-state p1))) *)
  ((goal-test p1 (sequence-result p1 (a*-graph-search p1) (problem-initial-state p1))) *)

  ((setq p2 *romania-problem*))
  ((goal-test p2 (sequence-result p2 (breadth-first-tree-search p2) (problem-initial-state p2))) *)
  ((goal-test p2 (sequence-result p2 (uniform-cost-tree-search p2) (problem-initial-state p2))) *)
  ;;; depth-first-tree-search goes into an infinite loop
  ((goal-test p2 (sequence-result p2 (iterative-deepening-search p2) (problem-initial-state p2))) *)
  ((goal-test p2 (sequence-result p2 (greedy-best-first-tree-search p2) (problem-initial-state p2))) *)
  ((goal-test p2 (sequence-result p2 (a*-tree-search p2) (problem-initial-state p2))) *)
  ((goal-test p2 (sequence-result p2 (ida* p2) (problem-initial-state p2))) *)
  ((goal-test p2 (sequence-result p2 (recursive-best-first-search p2) (problem-initial-state p2))) *)
  ((goal-test p2 (sequence-result p2 (breadth-first-graph-search p2) (problem-initial-state p2))) *)
  ((goal-test p2 (sequence-result p2 (uniform-cost-graph-search p2) (problem-initial-state p2))) *)
  ((goal-test p2 (sequence-result p2 (depth-first-graph-search p2) (problem-initial-state p2))) *)
  ((goal-test p2 (sequence-result p2 (greedy-best-first-graph-search p2) (problem-initial-state p2))) *)
  ((goal-test p2 (sequence-result p2 (a*-graph-search p2) (problem-initial-state p2))) *)

  ((setq p3 (make-route-finding-problem :n 5)))
  ((goal-test p3 (sequence-result p3 (breadth-first-tree-search p3) (problem-initial-state p3))) *)
  ((goal-test p3 (sequence-result p3 (uniform-cost-tree-search p3) (problem-initial-state p3))) *)
  ;;; depth-first-tree-search goes into an infinite loop
  ((goal-test p3 (sequence-result p3 (iterative-deepening-search p3) (problem-initial-state p3))) *)
  ((goal-test p3 (sequence-result p3 (greedy-best-first-tree-search p3) (problem-initial-state p3))) *)
  ((goal-test p3 (sequence-result p3 (a*-tree-search p3) (problem-initial-state p3))) *)
  ((goal-test p3 (sequence-result p3 (ida* p3) (problem-initial-state p3))) *)
  ((goal-test p3 (sequence-result p3 (recursive-best-first-search p3) (problem-initial-state p3))) *)
  ((goal-test p3 (sequence-result p3 (breadth-first-graph-search p3) (problem-initial-state p3))) *)
  ((goal-test p3 (sequence-result p3 (uniform-cost-graph-search p3) (problem-initial-state p3))) *)
  ((goal-test p3 (sequence-result p3 (depth-first-graph-search p3) (problem-initial-state p3))) *)
  ((goal-test p3 (sequence-result p3 (greedy-best-first-graph-search p3) (problem-initial-state p3))) *)
  ((goal-test p3 (sequence-result p3 (a*-graph-search p3) (problem-initial-state p3))) *)

  ((setq p4 (make-route-finding-problem :n 20)))
  ;;; the uninformed tree search methods take too long in some cases
  ;;; and depth-first-tree-search goes into an infinite loop
  ((goal-test p4 (sequence-result p4 (a*-tree-search p4) (problem-initial-state p4))) *)
  ((goal-test p4 (sequence-result p4 (ida* p4) (problem-initial-state p4))) *)
  ((goal-test p4 (sequence-result p4 (recursive-best-first-search p4) (problem-initial-state p4))) *)
  ((goal-test p4 (sequence-result p4 (breadth-first-graph-search p4) (problem-initial-state p4))) *)
  ((goal-test p4 (sequence-result p4 (uniform-cost-graph-search p4) (problem-initial-state p4))) *)
  ((goal-test p4 (sequence-result p4 (depth-first-graph-search p4) (problem-initial-state p4))) *)
  ((goal-test p4 (sequence-result p4 (greedy-best-first-graph-search p4) (problem-initial-state p4))) *)
  ((goal-test p4 (sequence-result p4 (a*-graph-search p4) (problem-initial-state p4))) *)

  ((setq p5 *Figure-3.4-8puzzle-problem*))
  ;;; most algorithms are too slow on this problem, which has solution length 26
  ((goal-test p5 (sequence-result p5 (a*-graph-search p5) (problem-initial-state p5))) *)

  ((setq p6 *Figure-4.17-path-planning-problem*))
  ((goal-test p6 (sequence-result p6 (breadth-first-tree-search p6) (problem-initial-state p6))) *)
  ((goal-test p6 (sequence-result p6 (uniform-cost-tree-search p6) (problem-initial-state p6))) *)
  ;;; depth-first-tree-search goes into an infinite loop
  ((goal-test p6 (sequence-result p6 (iterative-deepening-search p6) (problem-initial-state p6))) *)
  ((goal-test p6 (sequence-result p6 (greedy-best-first-tree-search p6) (problem-initial-state p6))) *)
  ((goal-test p6 (sequence-result p6 (a*-tree-search p6) (problem-initial-state p6))) *)
  ((goal-test p6 (sequence-result p6 (ida* p6) (problem-initial-state p6))) *)
  ((goal-test p6 (sequence-result p6 (recursive-best-first-search p6) (problem-initial-state p6))) *)
  ((goal-test p6 (sequence-result p6 (breadth-first-graph-search p6) (problem-initial-state p6))) *)
  ((goal-test p6 (sequence-result p6 (uniform-cost-graph-search p6) (problem-initial-state p6))) *)
  ((goal-test p6 (sequence-result p6 (depth-first-graph-search p6) (problem-initial-state p6))) *)
  ((goal-test p6 (sequence-result p6 (greedy-best-first-graph-search p6) (problem-initial-state p6))) *)
  ((goal-test p6 (sequence-result p6 (a*-graph-search p6) (problem-initial-state p6))) *)

  ((setq p7 (make-tsp :n 5)))
  ((goal-test p7 (sequence-result p7 (breadth-first-tree-search p7) (problem-initial-state p7))) *)
  ((goal-test p7 (sequence-result p7 (uniform-cost-tree-search p7) (problem-initial-state p7))) *)
  ;;; depth-first-tree-search goes into an infinite loop
  ((goal-test p7 (sequence-result p7 (iterative-deepening-search p7) (problem-initial-state p7))) *)
  ((goal-test p7 (sequence-result p7 (greedy-best-first-tree-search p7) (problem-initial-state p7))) *)
  ((goal-test p7 (sequence-result p7 (a*-tree-search p7) (problem-initial-state p7))) *)
  ((goal-test p7 (sequence-result p7 (ida* p7) (problem-initial-state p7))) *)
  ((goal-test p7 (sequence-result p7 (recursive-best-first-search p7) (problem-initial-state p7))) *)
  ((goal-test p7 (sequence-result p7 (breadth-first-graph-search p7) (problem-initial-state p7))) *)
  ((goal-test p7 (sequence-result p7 (uniform-cost-graph-search p7) (problem-initial-state p7))) *)
  ((goal-test p7 (sequence-result p7 (depth-first-graph-search p7) (problem-initial-state p7))) *)
  ((goal-test p7 (sequence-result p7 (greedy-best-first-graph-search p7) (problem-initial-state p7))) *)
  ((goal-test p7 (sequence-result p7 (a*-graph-search p7) (problem-initial-state p7))) *)

  ((setq p8 (make-nqueens-incremental-problem :n 4)))
  ((goal-test p8 (sequence-result p8 (breadth-first-tree-search p8) (problem-initial-state p8))) *)
  ((goal-test p8 (sequence-result p8 (uniform-cost-tree-search p8) (problem-initial-state p8))) *)
  ((goal-test p8 (sequence-result p8 (depth-first-tree-search p8) (problem-initial-state p8))) *)
  ((goal-test p8 (sequence-result p8 (iterative-deepening-search p8) (problem-initial-state p8))) *)
  ((goal-test p8 (sequence-result p8 (greedy-best-first-tree-search p8) (problem-initial-state p8))) *)
  ((goal-test p8 (sequence-result p8 (a*-tree-search p8) (problem-initial-state p8))) *)
  ((goal-test p8 (sequence-result p8 (ida* p8) (problem-initial-state p8))) *)
  ((goal-test p8 (sequence-result p8 (recursive-best-first-search p8) (problem-initial-state p8))) *)
  ((goal-test p8 (sequence-result p8 (breadth-first-graph-search p8) (problem-initial-state p8))) *)
  ((goal-test p8 (sequence-result p8 (uniform-cost-graph-search p8) (problem-initial-state p8))) *)
  ((goal-test p8 (sequence-result p8 (depth-first-graph-search p8) (problem-initial-state p8))) *)
  ((goal-test p8 (sequence-result p8 (greedy-best-first-graph-search p8) (problem-initial-state p8))) *)
  ((goal-test p8 (sequence-result p8 (a*-graph-search p8) (problem-initial-state p8))) *)

  ((setq p9 *simple-maze-problem*))
  ((goal-test p9 (sequence-result p9 (breadth-first-tree-search p9) (problem-initial-state p9))) *)
  ((goal-test p9 (sequence-result p9 (uniform-cost-tree-search p9) (problem-initial-state p9))) *)
  ;;; depth-first-tree-search goes into an infinite loop
  ((goal-test p9 (sequence-result p9 (iterative-deepening-search p9) (problem-initial-state p9))) *)
  ;;; greedy-best-first-tree-search goes into an infinite loop
  ((goal-test p9 (sequence-result p9 (a*-tree-search p9) (problem-initial-state p9))) *)
  ((goal-test p9 (sequence-result p9 (ida* p9) (problem-initial-state p9))) *)
  ((goal-test p9 (sequence-result p9 (recursive-best-first-search p9) (problem-initial-state p9))) *)
  ((goal-test p9 (sequence-result p9 (breadth-first-graph-search p9) (problem-initial-state p9))) *)
  ((goal-test p9 (sequence-result p9 (uniform-cost-graph-search p9) (problem-initial-state p9))) *)
  ((goal-test p9 (sequence-result p9 (depth-first-graph-search p9) (problem-initial-state p9))) *)
  ((goal-test p9 (sequence-result p9 (greedy-best-first-graph-search p9) (problem-initial-state p9))) *)
  ((goal-test p9 (sequence-result p9 (a*-graph-search p9) (problem-initial-state p9))) *)

  "Testing local search algorithms is more difficult; they do not
   guarantee a solution. We just make sure
   the algorithms terminate without errors."

  ((setq l1 (make-nqueens-complete-problem :n 8)))
  ((hill-climbing l1))
  ((hill-climbing l1 #'local-minimum))
  ((hill-climbing l1 (make-minimum-or-flat-n-times 10)))
  ((random-restart-hill-climbing #'(lambda () (make-nqueens-complete-problem :n 8))))
  ((simulated-annealing l1))
  ((local-beam-search #'(lambda () (make-nqueens-complete-problem :n 8))))
  ((local-beam-search #'(lambda () (make-nqueens-complete-problem :n 8)) 10 #'best-local-minimum))
  ((local-beam-search #'(lambda () (make-nqueens-complete-problem :n 8)) 10 (make-best-minimum-or-flat-n-times 10)))
  ((genetic-search #'(lambda () (make-nqueens-complete-problem :n 8))))

  ((setq l2 (make-sliding-block-puzzle :n 3)))
  ((hill-climbing l2))
  ((hill-climbing l2 #'local-minimum))
  ((hill-climbing l2 (make-minimum-or-flat-n-times 10)))
  ((random-restart-hill-climbing #'(lambda () (make-sliding-block-puzzle :n 3))))
  ((simulated-annealing l2))
  ((local-beam-search #'(lambda () (make-sliding-block-puzzle :n 3))))
  ((local-beam-search #'(lambda () (make-sliding-block-puzzle :n 3)) 10 #'best-local-minimum))
  ((local-beam-search #'(lambda () (make-sliding-block-puzzle :n 3)) 10 (make-best-minimum-or-flat-n-times 10)))

  "Now test construction of problem-solving environments and agents therein.
   Check that the final environment state meets the goal test."

  ((setq e1 (problem->environment p1)))
  ((run-environment e1) (goal-test p1 (environment-state *)))
  ((setq e2 (problem->environment p2)))
  ((run-environment e2) (goal-test p2 (environment-state *)))
  ((setq e3 (problem->environment p3)))
  ((run-environment e3) (goal-test p3 (environment-state *)))
  ((setq e4 (problem->environment p4)))
  ((run-environment e4) (goal-test p4 (environment-state *)))
  ((setq e5 (problem->environment p5)))
  ((run-environment e5) (goal-test p5 (environment-state *)))
  ((setq e6 (problem->environment p6)))
  ((run-environment e6) (goal-test p6 (environment-state *)))
  ((setq e7 (problem->environment p7)))
  ((run-environment e7) (goal-test p7 (environment-state *)))
  ((setq e8 (problem->environment p8)))
  ((run-environment e8) (goal-test p8 (environment-state *)))
  ((setq e9 (problem->environment p9)))
  ((run-environment e9) (goal-test p9 (environment-state *)))

  "Now test the online search agents. Except for the random-walk agent,
   these should reach the goal."

  ((setq p10 *simple-maze-problem*))
  ((setq e10-dfs (problem->environment p10 :agents (list (new-online-dfs-agent :problem p10)))))
  ((run-environment e10-dfs) (goal-test p10 (environment-state *)))
  ((setq e10-random (problem->environment p10 :agents (list (new-random-walk-agent :problem p10)))))
  ((run-environment e10-random))
  ((setq e10-lrta* (problem->environment p10 :agents (list (new-lrta*-agent :problem p10)))))
  ((run-environment e10-lrta*) (goal-test p10 (environment-state *)))

  "The random walk trap problem is not reversible, so DFS does not necessarily work."
  ((setq p11 (make-random-walk-trap-problem :n 11)))
  ((setq e11-random (problem->environment p11 :agents (list (new-random-walk-agent :problem p11)))))
  ((run-environment e11-random))
  ((setq e11-lrta* (problem->environment p11 :agents (list (new-lrta*-agent :problem p11)))))
  ((run-environment e11-lrta*) (goal-test p11 (environment-state *)))

  ((setq p12 *lrta-demo-problem*))
  ((setq e12-dfs (problem->environment p12 :agents (list (new-online-dfs-agent :problem p12)))))
  ((run-environment e12-dfs) (goal-test p12 (environment-state *)))
  ((setq e12-random (problem->environment p12 :agents (list (new-random-walk-agent :problem p12)))))
  ((run-environment e12-random))
  ((setq e12-lrta* (problem->environment p12 :agents (list (new-lrta*-agent :problem p12)))))
  ((run-environment e12-lrta*) (goal-test p12 (environment-state *)))

  )


