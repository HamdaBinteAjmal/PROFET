;;;; edit-nets.lisp -- functions to edit BNs and DBNs
;;;; Norm Aleks 2007-06-04
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
;;;; $Header: /Users/norm/lisp/aima/probability/domains/RCS/edit-nets.lisp,v 1.7 2008/03/11 18:32:01 norm Exp norm $

(defun help-edit-nets ()
  (format t 
"        add-node
        drop-node
        change-node-type
        rename-node
        
        add-parents
        drop-parents
        
        also useful:
        display-node
        create-node-distribution
        ... which may be done automatically with fix-distributions, maybe.
        
        clear-compiled-functions ... before you save
        dot-dbn
        display-links
"))

(defparameter *fix-distributions* NIL)

;;; TODO: This isn't right -- a node can remain in *fix-distributions* after, say, being
;;; removed from the net.  There's also nothing stopping us from editing several nets
;;; at once, so *fix-distributions* ought to be part of the bayes-net structure.  
;;; For each node, we should store the
;;; previous distribution (for ease of entry of the new one) and WHY a node's in here -- 
;;; a list of the parent changes or node type changes that led to its entry.

(defun add-to-fix-dist-list (ref &optional prior-dist-ref)
  (unless (member ref *fix-distributions*)
    (push ref *fix-distributions*)))
;;(format t "*fix-distributions* now contains ~{~A ~}~%"
;;   (mapcar #'bnode-name *fix-distributions*))))

(defun fix-distributions ()
  "Go through distribution entry process for each of the nodes in *fix-distributions*"
  (loop FOR node-ref = (pop *fix-distributions*) WHILE node-ref DO
       (format t "~&~%Node ~A's parents are now ~A.~%"
	       (bnode-name node-ref) (mapcar #'bnode-name (bnode-parents node-ref)))
       (format t "The previous distribution was not saved~%")
       (create-node-distribution node-ref))
  T)

(defun slice-generic-p (net name)
  "Returns T if the net's a DBN and the name is an unsubscripted node name within it"
  (and (eq 'DBN (type-of net))
       (member name (dbn-var-names net))))

(defun batch-create-node (name type &optional values)
  "Make a new BN node, name NAME, type TYPE, values VALUES if it's discrete"
  (when (member type '(:tabulated :arbitrary-discrete))
    (unless (and values
		 (listp values)
		 (every #'atom values) 
		 (equal values (remove-duplicates values)))
      (error "For TABULATED nodes, VALUES list must exist and consist of distinct atoms.")))
  (let ((ref (case type
                   (:typeless (make-typeless-bnode :name name))
                   (:tabulated (make-tabulated-bnode :name name))
                   (:deterministic (make-deterministic-bnode :name name))
                   (:arbitrary-continuous (make-arbitrary-continuous-bnode :name name))
                   (:arbitrary-discrete (make-arbitrary-discrete-bnode :name name))
                   (:noisy-or (make-noisy-or-bnode :name name))
                   (:linear-gaussian (make-linear-gaussian-bnode :name name))
                   (:truncated-gaussian (make-truncated-gaussian-bnode :name name))
                   (:uniform (make-uniform-bnode :name name))
                   (:probit (make-probit-bnode :name name))
	       (otherwise (error 
"Unknown type ~A -- need one of (:typeless :tabulated :deterministic :arbitrary-continuous 
:arbitrary-discrete :noisy-or :linear-gaussian :truncated-gaussian : uniform :probit)" type)))))
    (when (member type '(:tabulated :arbitrary-discrete))
      (setf (discrete-bnode-value-names ref) values)
      (setf (discrete-bnode-arity ref) (length values)))
    ref))

(defun change-node-type (net name type &optional values)
  "replace node name in net with a new node"
  (unless (if (eq 'DBN (type-of net))
	      (member name (dbn-var-names net))
	      (bnode-by-name name net))
    (error "No such node ~A to replace in this net" name))
  (let ((new (batch-create-node name type values)))
    (if (eq 'DBN (type-of net))
	(progn 
	  (change-node-type2 net name (copy-bnode new) 0)
	  (change-node-type2 net name new 1))
	(change-node-type2 net name new)))
  T)

(defun change-node-type2 (net old-name new &optional slice)
  "change all the pointers for one (slice's) node in the BN -- and in DBN structs if needed"
  (let ((old (bnode-by-name (if slice  (subscript old-name slice)  old-name)
			    net)))
    (setf (bnode-name new) (bnode-name old))
    (setf (bnode-index new) (bnode-index old))
    (setf (bnode-parents new) (bnode-parents old))
    (setf (bnode-children new) (bnode-children old))
    (loop FOR parent IN (bnode-parents new) DO
	 (setf (bnode-children parent) (substitute new old (bnode-children parent))))
    (loop FOR child IN (bnode-children new) DO
	 (setf (bnode-parents child) (substitute new old (bnode-parents child))))
    (setf (bn-nodes net) (substitute new old (bn-nodes net)))
    (when (= slice 0)
      (setf (dbn-slice0 net) (substitute new old (dbn-slice0 net))))
    (when (= slice 1)
      (setf (dbn-slice1 net) (substitute new old (dbn-slice1 net))))
    ;; no need to sort!
    (add-to-fix-dist-list new old)
    ;; children's distribs need correction if type changed between continuous and discrete
    (unless (eq (discrete-bnode-p old) (discrete-bnode-p new))
      (loop FOR child IN (bnode-children new) DO
	   (add-to-fix-dist-list child child)))))

(defun rename-node (net old-name new-name)
  "Rename the node currently named OLD-NAME to NEW-NAME"
  ;; for DBNs
  (when (eq 'DBN (type-of net))
    (unless (member old-name (dbn-var-names net))
      (error "No such node ~A in this DBN" old-name))
    (when (member new-name (dbn-var-names net))
      (error "Proposed new name ~A already exists in this DBN" new-name))
    (setf (dbn-var-names net) (substitute new-name old-name (dbn-var-names net)))
    (setf (bnode-name (bnode-by-name (subscript old-name 0) net)) (subscript new-name 0))
    (setf (bnode-name (bnode-by-name (subscript old-name 1) net)) (subscript new-name 1)))
  ;; for BNs
  (when (not (eq 'DBN (type-of net)))
    (unless (bnode-by-name old-name net)
      (error "No such node ~A in this DBN" old-name))
    (when (bnode-by-name new-name net)
      (error "Proposed new name ~A already exists in this DBN" new-name))
    (setf (bnode-name (bnode-by-name old-name net)) new-name))
  T)

(defun add-parents (net child parent-names)
  "Adds an arc to the node named child from its new (list of) parent-names in net"
  (if (slice-generic-p net child)
      (progn
	(unless (every #'(lambda (child) (slice-generic-p net child)) parent-names)
	  (error "Error: add-parents: can't mix unsubscripted and subscripted names"))
	(add-parents net (subscript child 0) 
		     (mapcar #'(lambda (child) (subscript child 0)) parent-names))
	(add-parents net (subscript child 1) 
		     (mapcar #'(lambda (child) (subscript child 1)) parent-names)))
      (loop FOR parent-name IN parent-names DO
	   (let ((node   (bnode-by-name child net))
		(parent (bnode-by-name parent-name net)))
	     (if (member parent (bnode-parents node))
		 (format t "Error: add-parents: ~A is already ~A's parent~%" parent-name child)
		 (progn
		   (push parent (bnode-parents node))
		   (push node (bnode-children parent))
		   ;;(push (bnode-index parent) (bnode-parent-indexes node)) ;;ce
		   (if (eq 'DBN (type-of net))
		       (sort-dbn net)
		       (sort-bayes-net net))
		   (add-to-fix-dist-list node))))))
  T)

(defun add-children (net parent children-names)
  "Adds arcs from node named name to its new children in net"
  ;; this is not a particularly elegant way to split up this problem
  (loop FOR child IN children-names DO
       (format t "~A -> ~A~%" parent child)
       (add-parents net child (list parent))))


(defun drop-parents (net name parent-names)
  "In named net, drops an arc to the node named name from its parents in list parent-names"
  ;; same basic routine as add-parent but no need to re-sort
  (if (slice-generic-p net name)
      (progn
	(unless (every #'(lambda (name) (slice-generic-p net name)) parent-names)
	  (error "Error: drop-parents: can't mix unsubscripted and subscripted names"))
	(drop-parents net (subscript name 0) 
		      (mapcar #'(lambda (name) (subscript name 0)) parent-names))
	(drop-parents net (subscript name 1) 
		      (mapcar #'(lambda (name) (subscript name 1)) parent-names)))
      (loop FOR parent-name IN parent-names DO
	   (let ((node   (bnode-by-name name net))
		 (parent (bnode-by-name parent-name net)))
	     (if (member parent (bnode-parents node))
		 (progn
		   (setf (bnode-parents node) (remove parent (bnode-parents node)))
		   (setf (bnode-children parent) (remove node (bnode-children parent)))
		   (add-to-fix-dist-list node))
		 (format t "Error: drop-parents: ~A is not ~A's parent~%" parent-name name)))))
  T)

(defun add-node (net name type &optional values)
  "non-interactively add a new node to the net, named NAME of type TYPE,
   with values VALUES if discrete."
  (when (if (eq 'DBN (type-of net))
	    (member name (dbn-var-names net))
	    (bnode-by-name name net))
    (error "There is already a node named ~A in this net" name))
  (let ((new (batch-create-node name type values)))
    (if (eq 'DBN (type-of net))
	;; for DBN, copy, rename as _0 and _1, add to nodes and slices and var-names
	(progn
	  (let ((new0 (copy-bnode new))
		(new1 new))
	    (setf (bnode-name new0) (subscript name 0))
	    (setf (bnode-name new1) (subscript name 1))
	    (add-to-fix-dist-list new0)
	    (add-to-fix-dist-list new1)
	    (push new0 (bn-nodes net))
	    (push new1 (bn-nodes net))
	    (push new0 (dbn-slice0 net))
	    (push new1 (dbn-slice1 net))
	    (push name (dbn-var-names net))))
	;; for a BN it's rather simpler
	(progn
	  (add-to-fix-dist-list new)
	  (push new (bn-nodes net)))))
  ;; renumber the net (any existing evidence vectors are useless now anyway)
  (create-indices net)
  T)

(defun drop-node (net name)
  "remove the node named NAME from the BN/DBN."
  (if (eq 'DBN (type-of net))
      (if (member name (dbn-var-names net))
	  (let ((ref0 (bnode-by-name (subscript name 0) net))
		(ref1 (bnode-by-name (subscript name 1) net)))
	    (drop-bn-node net ref0)
	    (setf (dbn-slice0 net) (remove ref0 (dbn-slice0 net)))
	    (drop-bn-node net ref1)
	    (setf (dbn-slice1 net) (remove ref1 (dbn-slice1 net)))
	    (setf (dbn-var-names net) (remove name (dbn-var-names net))))
	  (format t "Error: drop-node: no node named ~A (don't subscript)~%" name))
      (drop-bn-node net (bnode-by-name name net)))
  (create-indices net))
	
(defun drop-bn-node (net ref)
  "subfunction of drop-node -- remove node pointed to by ref from BN"
  (loop FOR parent IN (bnode-parents ref) DO
       (setf (bnode-children parent) (remove ref (bnode-children parent))))
  (loop FOR child IN (bnode-children ref) DO
       (drop-parents net (bnode-name child) (list (bnode-name ref))))
  (setf (bn-nodes net) (remove ref (bn-nodes net))))

(defun drop-nodes (net namelist)
  "Drops from net 'net' all the nodes named in 'namelist'"
  (loop FOR name IN namelist DO (drop-node net name)))


;;; edit-cpt-row by SJR, moved from bayes-nets.lisp

(defun edit-cpt-row (name bn &aux (bnode (bnode-by-name name bn))
		     (parents (bnode-parents bnode))
		     (parent-names (mapcar #'bnode-name parents))
		     (cpt (tabulated-bnode-cpt bnode))
		     indices)
  "Interactively modify the CPT entries for a particular setting of parent values.
   Return value is unspecified."
  (format t "~%Parents of ~A are ~A" name parent-names)
  (setf indices   
	(mapcar #'(lambda (parent parent-name)
                    (let* ((pvals (discrete-bnode-value-names parent))
                           (p (query-user
                               (format nil "~%Enter value of ~A: " parent-name)
                               #'(lambda (p) (member p pvals))
                               (format nil "~%Must be a member of ~A" pvals))))
                      (position p pvals  :test #'eq)))
                parents parent-names))
  (setf (apply #'aref cpt indices)
        (list->distribution
         (query-user (format nil "~%Enter probabilities for ~A = ~A: "
                             (bnode-name bnode) (discrete-bnode-value-names bnode))
                     #'(lambda (x) (and (listp x) (every #'probabilityp x)))
                     (format nil "~%Must be a list of numbers in [0,1]")))))

;;; Clear the compiled functions in a net, to make it saveable

(defun clear-compiled-functions (net)
  (loop FOR bnode IN (bn-nodes net) DO
       (typecase bnode
         (typeless-bnode
	  (setf (typeless-bnode-compiled-fn bnode) NIL))
	 (deterministic-bnode 
	  (setf (deterministic-bnode-compiled-fn bnode) NIL))
	 (arbitrary-continuous-bnode 
	  (setf (arbitrary-continuous-bnode-compiled-fn bnode) NIL))
	 (arbitrary-discrete-bnode 
	  (setf (arbitrary-discrete-bnode-compiled-fn bnode) NIL)))))


;;; DISPLAY A NET

(defun display-links (net &key (children T) (parents T))
  (loop FOR node IN (bn-nodes net) DO
       (format t "~A (type ~A)~%" (bnode-name node) (type-of node))
       (when parents
	 (format t "     Parents:  ~A~%" (mapcar #'bnode-name (bnode-parents node))))
       (when children 
	 (format t "     Children: ~A~%" (mapcar #'bnode-name (bnode-children node))))))



;;; Functions to generate a DOT file for graphing DBNs (not BNs).
;;; Michael Madden, June 2007 in UC Berkeley.
;;; Version 5, 17 June 2007.

;;; Important: DOT files are parsed using AT&T Lab's free Graphviz tools.
;;; To visualise the generated files, you need to install the graphviz 
;;; tools from http:///www.graphviz.org/ (or something compatible).

;;; For a DBN called mydbn, call:
;;;    (dot-dbn mydbn "mydbn" t)
;;; This will generate a file called "mydbn.dot". 
;;; Use NIL as the last argument to display a single time-slice rather than two.

;;; In your shell, you can view this file using:
;;;    dotty mydbn.dot
;;; To generate a postscript file of it:
;;;    dot -Tps mydbn.dot -o mydbn.ps
;;; To generate GIF:
;;;    dot -Tgif mydbn.dot -o mydbn.gif
;;; To generate PDF:
;;;    dot -Tpdf mydbn.dot -o mydbn.pdf

(defun dot-dbn (dbn filename &key (2slice NIL) (tooltips NIL)
                &aux (fullfile (concatenate 'string filename ".dot"))
                     (nodelist (if 2slice (bn-nodes dbn) (dbn-slice1 dbn)))) 
  "Writes out a DOT file, which can be displayed on screen using dotty filename.dot."
  ;; If 2slice = t, time slices 0 and 1 are both shown, in different clusters.
  ;; If only showing one slice, it's Slice 1, not Slice 0.

  (with-open-file (STR fullfile :direction :output :if-exists :supersede) 
    (format STR "~%/* Network generated by dot-dbn (first by M Madden) */~%~%Digraph DBN~%") 
    ;;(format STR "{~%orientation=\"landscape\"~%size=\"9,7\";~%ratio=fill;~%~%")
    (format STR "{~%resolution=52;~%ranksep=1;~%")
    ;;(format STR "{~%orientation=\"landscape\";~%~%")
    (format STR "node [style=filled, color=black];~%")
    (when 2slice 
      (dot-nodes-on-slice 0 dbn STR)
      (dot-nodes-on-slice 1 dbn STR))
    
    ;; TO DO: for the 2-slice case, should write sets of statements so that nodes 
    ;; on each slice will be at same vertical position:
    ;; { rank = same;   VAR_NAME_0;   VAR_NAME_1; }
    
    (loop for bnode in nodelist do
	 (dot-node dbn bnode STR 2slice tooltips))
    
    (format STR "}~%"))
  (format t "~%DBN written to ~A.~%" fullfile))


(defun dot-node (dbn bnode STR 2slice tooltips 
                 &aux (bnodename (bnode-name bnode)))
  "Writes DOT file text for a node and its parents."
  (let ((bnodename-as-str (nodename-to-str bnodename 2slice)))
    
    (when (not 2slice) 
      (format STR "~A [" bnodename-as-str)
      (cond 
        ((OR (string= bnodename-as-str "\"Ob" :start1 0 :end1 3 );;Observed
             (string= bnodename-as-str "\"Pr" :start1 0 :end1 3 );;Prescribed
             (string= bnodename-as-str "\"In" :start1 0 :end1 3 ));;Intended
              (format STR "color=black fontcolor=white fontsize=21")) ;; Evidence nodes should be black
        ((member (type-of bnode) '(deterministic-bnode))
         ;;(format STR "color=black fontcolor=black fontsize=25 shape=ellipse style=solid"))
         (format STR "fillcolor=gray89 fontcolor=black fontsize=21 "))
        ((member (type-of bnode)
                 '(arbitrary-continuous-bnode arbitrary-discrete-bnode))
         (format STR "color=gray70 fontcolor=cadetblue4 fontsize=21"))
        (T (format STR "color=grey69 fontcolor=black fontsize=21")))
      
      (format STR " fontname=Helvetica];~%"))
    (loop for parent in (mapcar #'bnode-name (bnode-parents bnode)) do
         (format STR "~A -> ~A [~A];~%" 
                 (nodename-to-str parent 2slice) bnodename-as-str
                 (if (difftime parent bnodename)
                     (if 2slice
                         "color=black constraint=false"
                         "color=black constraint=false style=\"bold, dashed\"")
                     "color=black"))))
)


(defun dot-nodes-on-slice (time dbn STR
                           &aux nstr 
                           (nodelist (if (= time 0) (dbn-slice0 dbn) (dbn-slice1 dbn))) 
		           (ttxt (format nil "~A" time))) ;; only called when 2slice = t
  "Defines a time-slice cluster containing  all nodes on it for a DOT file."
  (format STR "subgraph cluster_~A {~%  style=filled;~%  color=~A;~%" 
	  ttxt (if (= time 0) "cyan" "gold"))
  (format STR "  node [style=filled, color=white];~%")
  (loop for nname in (mapcar #'bnode-name nodelist) do
       (setq nstr (nodename-to-str nname t))
       (when (string/= (char nstr (- (length nstr) 2)) ttxt) 
	 (format t " !! ERROR: nodelist contains ~A but time is ~A !!~%" nname time))
       (format STR "  ~A;~%" nstr))
  (format STR "  label = \"T=~A\";~%}~%~%" ttxt))


(defun nodename-to-str (nodename incl_subscript)
  (let ((ntext (format NIL "~A" (if incl_subscript
                                    nodename
                                    (unsubscript nodename)))))
    (string-capitalize (substitute #\Space #\- (format nil "\"~A\"" ntext)))))

(defun sym-to-str (sym) 
  (format nil "~A" sym))

(defun difftime (nodeA nodeB &aux (strA (sym-to-str nodeA)) (strB (sym-to-str nodeB)))
  (char/= (char strA (- (length strA) 1)) (char strB (- (length strB) 1))))
