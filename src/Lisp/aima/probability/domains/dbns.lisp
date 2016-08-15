;;; dbn.lisp
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
;;; Definitions for general DBNs. 
;;; Represented as a subclass of BNs.
;;; If var-names is, say, (X Y Z), then slice0 is (X0 Y0 Z0)
;;; and slice1 is (X1 Y1 Z1)

(declaim (optimize (debug 0))) ;; lets slime/SBCL do function tracebacks

(defstruct (dbn (:include bn))
  var-names	           ;;; root names for variables in slices.
  slice0		   ;;; topologically ordered list of variables in slice 0
  slice1		   ;;; topologically ordered list of variables in slice 1
  )

(defun create-dbn ()
  (format t "~%*****************Creating DBN*******************~%")
  (format t "~%You will be asked first to create the nodes. ~%Enter node names for ONE SLICE ONLY and do not add temporal indices to the names.~%Nodes for time slices 0 and 1 will be created automatically.~%Use the subscripted names (X_0, E_1 etc.) when specifying parents.~%")
  (create-distributions
   (sort-dbn ;; creates the indices and does a consistency check
    (create-arcs
     (multiple-value-bind (names slice0 slice1) (create-dbn-nodes)
       (make-dbn :nodes (append slice0 (copy-list slice1))
		 :slice0 slice0 :slice1 slice1
		 :var-names names))))))

(defun create-dbn-nodes (&aux (names nil) (bnode0 t) (slice0 nil) (slice1 nil))
  "Returns a list of BN nodes created interactively."
  (loop until (null bnode0) do
    (setf bnode0 (create-node-interactively))
    (when bnode0
      (let ((name (bnode-name bnode0))
	    (bnode1 (copy-bnode bnode0)))
	(push name names)
	(setf (bnode-name bnode0) (subscript name 0))
	(setf (bnode-name bnode1) (subscript name 1))
	(push bnode0 slice0) 
	(push bnode1 slice1))))
  (values names slice0 slice1))


(defun subscript (name k)
"Takes a symbol and integer k and returns a new symbol name_k."
  (intern (concatenate 'string (symbol-name name) "_" (princ-to-string k))))

(defun unsubscript (name_k)
  "Takes a symbol name_k and returns the original symbol name."
  (let ((name_k-string (symbol-name name_k)))
    (intern (subseq name_k-string 0 (position #\_ name_k-string :from-end t)))))


;; below written by SJR.  It is buggy in that the nodes in slice0 and slice1 can have
;; different orderings, even though other parts of the code assume that the nodes
;; are in the same order in each slice.
;(defun sort-dbn (dbn)
;  "Return DBN with its nodes sorted topologically according to parent links.
;   We enforce the odering between slice0 and slice1 by sorting separately."
;  (setf (dbn-slice0 dbn) (topological-BN-sort (dbn-slice0 dbn)))
;  (setf (dbn-slice1 dbn) (topological-BN-sort (dbn-slice1 dbn)))
;  (setf (bn-nodes dbn) (append (dbn-slice0 dbn) (copy-list (dbn-slice1 dbn))))
;  dbn)


;;; code from here down to the end of dbn-sort by Norm Aleks 12/2006

(defun 0-subscripted? (name)
  "Does the symbol name end in _0?"
  (let ((str (symbol-name name)))
    (and (char= #\_ (char str (- (length str) 2)))
	 (char= #\0 (char str (1- (length str)))))))

(defun 1-subscripted? (name)
  "Does the symbol name end in _1?"
  (let ((str (symbol-name name)))
    (and (char= #\_ (char str (- (length str) 2)))
	 (char= #\1 (char str (1- (length str)))))))

(defun no-parents? (node)
  (null (bnode-parents node)))

(defun sort-dbn (dbn)
  "Returns DBN with its nodes sorted topologically according to parent links, and the nodes in the two slices in identical order.  Also does some sanity checks of the DBN."
  ;; this is all pretty inefficient, O(n^2).  Should be possible to do it all in linear time,
  ;; probably with some juggling of data structures, but first see if it matters.  Probably 
  ;; doesn't on any reasonably-sized DBN, given that it runs only once per editing command.
  (let ((base-names (dbn-var-names dbn))
	(slice0 (dbn-slice0 dbn))
	(slice1 (dbn-slice1 dbn))
	;(all-nodes (append (dbn-slice0 dbn) (copy-list (dbn-slice1 dbn))))
	;; for sort we create a temporary graph using an assoc-list held in lower-layers
	;; and top-layer, each node containing a name and a list of parent names
	top-layer lower-layers sorted)
    ;; var-names, slice0, and slice1 must all have the same base set of names.  Check that:
    ;; 1) there are no dups in the base names
    ;; 2) the three lists have the same number of entries
    ;; 3) each name in each slice is appropriately suffixed with _0 or _1
    ;; 4) each base name has corresponding _0 and _1 names
    ;; 5) SHOULD also check that no nodes in slice 0 list nodes in slice 1 as parents
    (unless (= (length base-names) (length (remove-duplicates base-names)))
      (error "Malformed DBN: repetition of names in var-names."))
    (unless (= (length base-names) (length slice0) (length slice1))
      (error "Malformed DBN: var-names, slice0, and slice1 are not identical lengths."))
    (let ((slice0names (mapcar #'bnode-name slice0))
	  (slice1names (mapcar #'bnode-name slice1)))
      (unless (and (every #'0-subscripted? slice0names) (every #'1-subscripted? slice1names))
	(error "Malformed DBN: node names don't all end in _0 or _1 as appropriate."))
      (unless (every #'(lambda (base) (find (subscript base 0) slice0names)) base-names)
	(error "Malformed DBN: the var names are different from the slice0 names."))
      (unless (every #'(lambda (base) (find (subscript base 1) slice1names)) base-names)
	(error "Malformed DBN: the var names are different from the slice1 names.")))
    (setf (dbn-nodes dbn) (append (dbn-slice0 dbn) (copy-list (dbn-slice1 dbn))))
    ;; create the temporary graph
    (loop FOR base IN base-names DO
	 (let ((node0 (bnode-by-name (subscript base 0) dbn))
	       (node1 (bnode-by-name (subscript base 1) dbn)))
	   (push (cons base
		       (remove-duplicates 
			(mapcar #'unsubscript 
				(append (mapcar #'bnode-name (bnode-parents node0))
					(remove-if #'0-subscripted? 
						   (mapcar #'bnode-name 
							   (bnode-parents node1)))))))
		 lower-layers)))
    ;; find the initial top layer (no parents)
    (setf top-layer (mapcar #'car (remove-if #'cdr lower-layers)))
    (setf lower-layers (remove-if-not #'cdr lower-layers))
    ;; now repeatedly pull nodes from top-layer and eliminate them from other nodes' parents.
    (loop UNTIL (null top-layer) DO
	 (let ((n (pop top-layer)))
	   (push n sorted)
	   (loop FOR m IN lower-layers DO
		(setf (cdr m) (remove n (cdr m)))))
	 ;; move any nodes in lower-layers that now have no parents to top-layer
	 (setf top-layer (append top-layer (mapcar #'car (remove-if #'cdr lower-layers))))
	 (setf lower-layers (remove-if-not #'cdr lower-layers)))
    ;; if any nodes remain in lower-layers we failed because there was a cycle
    (unless (null lower-layers)
      (error "Can't topologically sort the DBN's slices simultaneously."))
    ;; store the ordered listings
    (setf sorted (reverse sorted))
    (setf (dbn-var-names dbn) sorted)
    (setf (dbn-slice0 dbn) 
	  (mapcar #'(lambda (base) (bnode-by-name (subscript base 0) dbn)) sorted))
    (setf (dbn-slice1 dbn)
	  (mapcar #'(lambda (base) (bnode-by-name (subscript base 1) dbn)) sorted))
    (setf (dbn-nodes dbn) (append (dbn-slice0 dbn) (copy-list (dbn-slice1 dbn)))))
  ;; finally, update the indices
  (loop FOR i FROM 0 TO (1- (length (dbn-nodes dbn)))
       AND node IN (dbn-nodes dbn) DO
       (setf (bnode-index node) i))
  dbn)

	 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; unroll-dbn takes a DBN and produces an unrolled network with slices 0...k.
;;; Slice 0 is a copy of the DBN's slice 0.
;;; Slices 1..k are copies of the DBN's slice 1.
;;; CPTs, parameters, etc., will be shared by the slices. All nodes will be new.
;;; The trick is to work out which nodes in layer i-1 and i are parents of nodes in
;;; layer i. Parent information is in the DBN, but recorded there in terms of
;;; slice 0 and slice 1 variables. Hence, when adding slice i we construct
;;; the mapping from slice 0/1 variables to slice i-1/i variables.

(defun unroll-dbn (dbn k &aux (bn (make-bn :nodes nil)) mapping)
  "Returns the Bayes net corresponding to the DBN unrolled for t=0...k."
  (let ((new-slice0 (mapcar #'(lambda (var) (copy-dbn-var var 0))
			    (dbn-slice0 dbn))))
    (setf mapping (mapcar #'cons (dbn-slice0 dbn) new-slice0))
    (loop for new in new-slice0 do (add-dbn-arcs new mapping))
    (setf (bn-nodes bn) new-slice0))
  (loop for i from 1 to k do
    (let ((new-slicei (mapcar #'(lambda (var) (copy-dbn-var var i))
			     (dbn-slice1 dbn))))
      (when (> i 1)
	(setf mapping (remove-if #'(lambda (old.new)
				     (member (car old.new) (dbn-slice0 dbn)))
				 mapping))
	(loop for old_1.new_i-1 in mapping do
	      (setf (car old_1.new_i-1)
		    (bnode-by-name (subscript (unsubscript (bnode-name (car old_1.new_i-1))) 0) dbn))))
      (setf mapping (append (mapcar #'cons (dbn-slice1 dbn) new-slicei) mapping))
      (loop for new in new-slicei do (add-dbn-arcs new mapping))
      (setf (bn-nodes bn) (append (bn-nodes bn) new-slicei))))
  (let ((j 0)) (loop for bnode in (bn-nodes bn) do (setf (bnode-index bnode) j) (incf j)))
  bn)


(defun copy-dbn-var (old i)
  "Return a copy of original DBN node with new subscript in name."
  (let ((new (copy-bnode old)))
    (setf (bnode-name new) (subscript (unsubscript (bnode-name old)) i))
    (setf (bnode-children new) nil)
    new))
       
(defun add-dbn-arcs (new mapping)
  "Add parents for new node, and add new to their children."
  (let ((old (cdr (rassoc new mapping))))
    (setf (bnode-parents new)
	  (mapcar #'(lambda (oldp) 
		      (let ((newp (cdr (assoc oldp mapping))))
			(push new (bnode-children newp))
			newp))
		  (bnode-parents old)))))




