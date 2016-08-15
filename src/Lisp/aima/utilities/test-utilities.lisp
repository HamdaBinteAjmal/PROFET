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

;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: utilities/test.lisp

;;;; Test cases for the basic utilities

(deftest utilities
  "Test all the utility functions."

  "Some operations on lists."
  ((length>1 '(a b c)) *)
  ((length>1 '(a)) (not *))
  ((length=1 '(a)) *)
  ((length=1 '(a b)) (not *))
  ((length=1 '(a . b)) (not *))
  ((random-element '(a b c)) (member * '(a b c)))
  ((random-index-if #'oddp '(0 1 2 3 4 5)) (oddp (elt '(0 1 2 3 4 5) *)))
  ((mappend #'reverse '((a b c) (1 2 3))) (equal * '(c b a 3 2 1)))
  ((starts-with '(hi there) 'hi) *)
  ((last1 '(a b c)) (eq * 'c))
  ((afind 'a '((a . 2) (b . 3))) 2)
  ((sum '(1 2 3) #'square) (= * 14))
  ((product '(1 2 3) #'square) (= * 36))
  ((remove-nth 1 '(a b c)) (equal * '(a c)))
  ((remove-nths '(2 4 0) '(a b c d e f)) (equal * '(b d f)))
  ((remove-sorted-nths '(0 2 4) '(a b c d e f)) (equal * '(b d f)))
  ((list-difference '(a b c d) '(b d)) (equal * '(a c)))
  ((same-setp '(a b c d) '(b b d a c)) *)
  ((left-rotate '(a b c)) (equal * '(b c a)))
  ((right-rotate '(a b c)) (equal * '(c a b)))
  ((insert-between 1 '(a b c)) (equal * '(a 1 b 1 c)))
  ((shuffle '(a b c d)) (and (subsetp * '(a b c d)) (subsetp '(a b c d) *)))
  ((transpose '((a b c) (d e f))) (equal * '((a d) (b e) (c f))))
  ((reuse-cons 'a 'b '(a . b)) (and (eq (car *) 'a) (eq (cdr *) 'b)))
  ((unique-find-anywhere-if #'numberp '(a (b 1 c) (3 (3 . d)))) (equal * '(1 3)))
  ((setf l '(a b c)))
  ((deletef 'a l) (equal l '(b c)))
  ((setf nums '(1 2 3 4 4 -5 -5 -2 -1)))
  ((the-biggest #'identity nums) (eql * 4))
  ((the-biggest #'abs nums) (eql * -5))
  ((the-biggest-random-tie #'identity nums) (eql * 4))
  ((the-biggest-that #'identity #'oddp nums) (eql * 3))
  ((the-smallest #'identity nums) (eql * -5))
  ((the-smallest #'abs nums) (member * '(1 -1)))
  ((the-smallest-random-tie #'identity nums) (eql * -5))
  ((the-smallest-that #'identity #'evenp nums) (eql * -2))


  "Now for 2-dimensional points."
  ((setq b (@ 1 2) c (@ 4 6)))
  ((xy-p b) *)
  ((xy-p (@ b b)) (not *))
  ((xy-equal b (@ 1 2)) *)
  ((xy-equal b c) (not *))
  ((xy-add b c) (xy-equal * (@ 5 8)))
  ((xy-diff c b) (xy-equal * (@ 3 4)))
  ((xy-scale b 2) (xy-equal * (@ 2 4)))
  ((xy-length b) (= * (sqrt 5)))
  ((xy-unit b) (xy-equal (xy-scale * (xy-length b)) b))
  ((xy-distance b c) (= * 5))
  ((x+y-distance b c) (= * 7))
  ((xy-between (xy-scale b 2) b (xy-scale b 3)) *)
  ((xy-between b (xy-scale b 2) (xy-scale b 3)) (not *))
  ((xy-angle (@ 3 0)) (= * 0.0))
  ((xy-relative-angle (@ 1 1) (@ 3 3)) (= * 0.0))

  "Expressions"
  ((setq exp (make-exp 'f 'a 'b)))
  ((op exp) (eq * 'f))
  ((args exp) (equal * '(a b)))
  ((arg1 exp) (eq * 'a))
  ((arg2 exp) (eq * 'b))
  ((setf (args exp) '(c d)) (equal (args exp) '(c d)))
  ((prefix->infix '(+ a b c d (+ e f))) (equal * '(a + b + c + d + (e + f))))

  

  "Numeric utilities"
  ((average '(10 20 30)) (= * 20))
  ((running-average 10 40 2) (= * 20))
  ((square -3) (= * 9))
  ((rms-error '(2 2 2) '(1 1 1)) (= * 1))
  ((ms-error '(2 2 2) '(1 1 1)) (= * 1))
  ((boolean-error '(2 2 2) '(1 1 1)) (= * 1))
  ((dot-product '(2 2 2) '(1 1 1)) (= * 6))
  ((iota 3) (equal * '(0 1 2)))
  ((random-integer 8 10) (member * '(8 9 10)))
  ((sample-with-replacement 2 '(a b c d e)) (and (= (length *) 2) (subsetp * '(a b c d e))))
  ((sample-without-replacement 2 '(a b c d e)) (and (= (length *) 2) (subsetp * '(a b c d e))))
  ((fuzz 10) (<= 9 * 11))
  ((round-off 3.14159 .01) (< 3.139 * 3.141))

  "Hash Tables"
  ((setq h (alist->hash-table '((a . 1) (b . 2) (c . nil)))))
  ((hash-table->alist h) (same-setp * '((a . 1) (b . 2) (c . nil)) :test #'equal))
  ((copy-hash-table h) (same-setp (hash-table->alist h) (hash-table->alist *) :test #'equal))
  ((in-hash-table? 'c h) *)
  ((in-hash-table? 'd h) (not *))
  ((null-hash-table-entry? 'c h) *)

  "Other"
  ((stringify '(a b c)) (equalp * "(A B C)"))
  ((concat-symbol 'a 1) (eq * 'a1))
  ((spaces 3) (equalp * "   "))
  ((dots 3) (equalp * "..."))
  ((truth '(a b c)) (eq * t))
  ((xor t nil) *)
  ((make 'array 3) (arrayp *))
  ((funcall (compose #'- #'sqrt) 16) (= * -4))

  "Now test the priority queue code."
  ((heap-sort '(1 4 3 5 2 0)) (equal * '(0 1 2 3 4 5)))
  ((heap-sort '(1 4 3 5 2 6) :key #'-) (equal * '(6 5 4 3 2 1)))

  )
