;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: utilities/queue.lisp

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

;;;; The Queue datatypes [2e p71]

;;; The operations on queues are as follows:
;;; Make-Queue(elements) creates a queue with the given list of elements.
;;;    (This is not used explicitly in the book; here, we use it only
;;;     to make an empty queue of the appropriate type.)
;;; Empty?(queue) returns true only if there are no elements in the queue.
;;; First(queue) returns the first element of the queue.
;;; Remove-First(queue) returns \prog{First}(queue) and removes it from the queue.
;;; Insert(element, queue) inserts an element and returns the resulting queue.  
;;;    (Different types of queues insert elements in different orders.)
;;; Insert-All(elements, queue) inserts a set of elements and returns the resulting queue.
;;;    (This is a nondestructive operation for the elements argument.)

;;; For search algorithms, we require three subtypes:
;;;   FIFO queue for breadth-first search
;;;   LIFO queue for depth-first search
;;;   priority queue for uniform-cost, best-first, etc., with a specified ordering key

;;; A FIFO queue puts new elements at the end and removes elements from the front.
;;; The implementation is a list with an extra pointer to the last cons cell.


(defstruct (FIFO-queue (:constructor create-FIFO-queue))
  elements        ;;; A list of elements
  last            ;;; The last cons cell of the elements list.
  )

(defun make-FIFO-queue (elements)
  "Constructs and returns a FIFO queue with the given elements."
  (let ((queue (create-FIFO-queue :elements (list nil))))
    (setf (FIFO-queue-last queue) (FIFO-queue-elements queue))
    (insert-all elements queue)
    queue))

(defmethod empty? ((queue FIFO-queue))
  "Are there no elements in the queue?"
  (null (rest (FIFO-queue-elements queue))))

(defmethod first-element ((queue FIFO-queue))
  "Return the element at the front of the queue."
  (if (empty? queue) (error "~%utilities/queue: first-element: Queue has no elements - ~A" queue)
    (first (FIFO-queue-elements queue))))

(defmethod remove-first ((queue FIFO-queue))
  "Remove the element from the front of the queue and return it."
  (if (empty? queue) (error "~%utilities/queue: remove-first: Queue has no elements - ~A" queue)
    (pop (FIFO-queue-elements queue))))

(defmethod insert (element (queue FIFO-queue))
  "Inserts an element and returns the resulting queue."
  (setf (first (FIFO-queue-last queue)) element)
  (setf (rest (FIFO-queue-last queue)) (list nil))
  (setf (FIFO-queue-last queue) (rest (FIFO-queue-last queue)))
  queue)

(defmethod insert-all (elements (queue FIFO-queue))
  "Inserts a set of elements and returns the resulting queue."
  (loop for element in elements do
    (insert element queue))
  queue)


;;; A LIFO queue (aka "stack") puts new elements on the front
;;; and removes elements from the front. The implementation
;;; is just an ordinary list.

(defstruct (LIFO-queue (:constructor create-LIFO-queue))
  elements        ;;; A list of elements
  )

(defun make-LIFO-queue (elements)
  "Constructs and returns a LIFO queue with the given elements."
  (create-LIFO-queue :elements elements))

(defmethod empty? ((queue LIFO-queue))
  "Are there no elements in the queue?"
  (null (LIFO-queue-elements queue)))

(defmethod first-element ((queue LIFO-queue))
  "Return the element at the front of the queue."
  (if (empty? queue) (error "~%utilities/queue: first-element: Queue has no elements - ~A" queue)
    (first (LIFO-queue-elements queue))))

(defmethod remove-first ((queue LIFO-queue))
  "Remove the element from the front of the queue and return it."
  (if (empty? queue) (error "~%utilities/queue: remove-first: Queue has no elements - ~A" queue)
    (pop (LIFO-queue-elements queue))))

(defmethod insert (element (queue LIFO-queue))
  "Inserts an element and returns the resulting queue."
  (push element (LIFO-queue-elements queue))
  queue)

(defmethod insert-all (elements (queue LIFO-queue))
  "Inserts a set of elements and returns the resulting queue."
  (setf (LIFO-queue-elements queue)
	(append elements (LIFO-queue-elements queue)))
  queue)


;;; A priority queue maintains elements in order sorted by
;;; a key. A specialized data structure is used to make insertion
;;; and deletion efficient. Here, we use the "heap" data structure
;;; as described by Cormen, Lieserson & Rivest [CL&R], "Introduction 
;;; to Algorithms," Chapter 7.  We could certainly speed
;;; up the constant factors of this implementation.  It is meant to be clear
;;; and simple and O(log n), but is not especially efficient.  Consider a Fibonacci
;;; heap [CL&R Page 420] if you really have large queues to deal with.


(defstruct (priority-queue (:constructor create-priority-queue))
  elements        ;;; A list of elements
  key             ;;; A function from elements to reals.
  )

(defun make-priority-queue (elements key)
  "Constructs and returns a priority queue, ordered by key, with the given elements."
  (let ((queue (create-priority-queue :elements (make-heap):key key)))
    (insert-all elements queue)
    queue))

(defmethod empty? ((queue priority-queue))
  "Are there no elements in the queue?"
  (= (length (priority-queue-elements queue)) 0))

(defmethod first-element ((queue priority-queue))
  "Return the element at the front of the queue."
  (elt (priority-queue-elements queue) 0))

(defmethod remove-first ((queue priority-queue))
  "Remove the element from the front of the queue and return it."
  (heap-extract-min (priority-queue-elements queue) (priority-queue-key queue)))

(defmethod insert (element (queue priority-queue))
  "Inserts an element and returns the resulting queue."
  (heap-insert element (priority-queue-elements queue) (priority-queue-key queue))
  queue)

(defmethod insert-all (elements (queue priority-queue))
  "Inserts a set of elements and returns the resulting queue."
  (loop for element in elements do
    (insert element queue))
  queue)


;;;; The Heap Implementation of Priority Queues

;;; The idea is to store a heap in an array so that the heap property is
;;; maintained for all elements: heap[Parent(i)] <= heap[i].  Note that we
;;; start at index 0, not 1, and that we put the lowest value at the top of
;;; the heap, not the highest value.

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))

;; These could be made inline

(defun heap-val (heap i key) (declare (fixnum i)) (funcall key (aref heap i)))
(defun heap-parent (i) (declare (fixnum i)) (floor (- i 1) 2))
(defun heap-left (i) (declare (fixnum i)) (the fixnum (+ 1 i i)))
(defun heap-right (i) (declare (fixnum i)) (the fixnum (+ 2 i i)))

(defun heapify (heap i key)
  "Assume that the children of i are heaps, but that heap[i] may be 
  larger than its children.  If it is, move heap[i] down where it belongs.
  [CL&R Page 143]."
  (let ((l (heap-left i))
	(r (heap-right i))
	(N (- (length heap) 1))
	smallest)
    (setf smallest (if (and (<= l N) (<= (heap-val heap l key)
					 (heap-val heap i key)))
		       l i))
    (if (and (<= r N) (<= (heap-val heap r key) (heap-val heap smallest key)))
	(setf smallest r))
    (when (/= smallest i)
      (rotatef (aref heap i) (aref heap smallest))
      (heapify heap smallest key))))

(defun heap-extract-min (heap key)
  "Pop the best (lowest valued) item off the heap. [CL&R Page 150]."
  (let ((min (aref heap 0)))
    (setf (aref heap 0) (aref heap (- (length heap) 1)))
    (decf (fill-pointer heap))
    (heapify heap 0 key)
    min))

(defun heap-insert (item heap key)
  "Put an item into a heap. [CL&R Page 150]."
  ;; Note that ITEM is the value to be inserted, and KEY is a function
  ;; that extracts the numeric value from the item.
  (vector-push-extend nil heap)
  (let ((i (- (length heap) 1))
	(val (funcall key item)))
    (loop while (and (> i 0) (>= (heap-val heap (heap-parent i) key) val)) do
      (setf (aref heap i) (aref heap (heap-parent i))
	    i (heap-parent i)))
    (setf (aref heap i) item)))

(defun heap-sort (numbers &key (key #'identity))
  "Return a sorted list, with elements that are < according to key first."
  ;; Mostly for testing the heap implementation
  ;; There are more efficient ways of sorting (even of heap-sorting)
  (let ((heap (make-heap))
	(result nil))
    (loop for n in numbers do (heap-insert n heap key))
    (loop while (> (length heap) 0) do (push (heap-extract-min heap key) result))
    (nreverse result)))
