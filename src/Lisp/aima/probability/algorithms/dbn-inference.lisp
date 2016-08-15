
;;;; $Header: /Users/norm/lisp/aima/probability/algorithms/RCS/dbn-inference.lisp,v 1.10 2008/05/27 03:14:25 norm Exp $
;;;; dbn-inference.lisp
;;;; originally by SJR, subsequent edits by Norm Aleks
;;;; CE added support for sub steps
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
(defun unroll-filter-by-name (Xname e_0-to-k dbn &optional (ask-by-name #'elimination-ask-by-name))
  "Returns a list of posterior distributions for unsubscripted Xname given e_0-to-i, 
   for time steps i=0...k where k is determined from the length of e_0-to-k."
  (let* ((slice-size (length (dbn-var-names dbn)))
         (k (1- (/ (length e_0-to-k) slice-size)))
         (e_0-to-i (make-sequence 'vector (length e_0-to-k) :initial-element nil))
         (start 0)
         (bn (unroll-dbn dbn k))
         (results nil))
    (loop for i from 0 to k do
          (loop for j from start to (1- (+ start slice-size)) do
                (setf (aref e_0-to-i j) (aref e_0-to-k j)))
          (incf start slice-size)
          (push (funcall ask-by-name (subscript Xname i) e_0-to-i bn) results))
    (reverse results)))

(defun lw-filter-by-name (Xname e_0-to-k dbn &optional (N 1000))
  "Returns a list of posterior distributions for unsubscripted Xname given e_0-to-k, 
   using likelihood weighting as the inference method on the unrolled network."
  (unroll-filter-by-name Xname e_0-to-k dbn 
                         #'(lambda (Xname e bn) (likelihood-weighting-ask-by-name Xname e bn N))))


(defun copy-particles (particles)
  (let* ((particle-count (length particles))
         (var-count (length (aref particles 0)))
         (archive (make-sequence 'vector particle-count)))
    (format t "record-particles: particle-count=~A, var-count=~A~%"
            particle-count var-count)
    (loop FOR i FROM 0 TO (1- particle-count) DO
          (setf (aref archive i) (make-sequence 'vector var-count))
          (let ((particle (aref particles i))
                (copy (aref archive i)))
            (loop FOR j FROM 0 TO (1- var-count) DO
                  (setf (aref copy j) (aref particle j)))))
    archive))


#|(defmethod summarize-bnode ((bnode discrete-bnode) particles weights)
(unless (= (length particles) (length weights))
(error "particles and weights vectors are of unequal length"))
(let ((i (bnode-index bnode))
(dist (make-distribution (discrete-bnode-arity bnode) 0.0d0)))
(loop FOR p ACROSS particles FOR w ACROSS weights DO
(increment-distribution dist (aref p i) w))
(distribution->alist (normalize dist) bnode)))

(defmethod summarize-bnode ((bnode bnode) particles weights)
;; continuous bnodes:  xbar_w = weighted mean; s^2_w = weighted variance
(unless (= (length particles) (length weights))
(error "particles and weights vectors are of unequal length"))
(let ((i (bnode-index bnode))
(s^2_w 0.0d0)
(xbar_w 0.0d0)
(wsum (loop FOR w ACROSS weights SUM w))
(nonzero-w-count (loop FOR w ACROSS weights UNLESS (zerop w) SUM 1)))
(setf xbar_w (/ (loop FOR p ACROSS particles FOR w ACROSS weights 
SUM (* w (aref p i))) 
wsum))
(setf s^2_w (/ (loop FOR p ACROSS particles FOR w ACROSS weights
SUM (*  w  (expt (- (aref p i) xbar_w) 2)))
(/ (* (1- nonzero-w-count) wsum)
nonzero-w-count)))
(list xbar_w (sqrt s^2_w))))|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARTICLE FILTERING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro -- (rest) 
  `(1- ,rest))

(defmethod summarize-bnode ((bnode discrete-bnode) ps ws)
  
  (unless (= (length ps) (length ws))
    (error "ps and ws vectors are of unequal length"))
  (let ((i (bnode-index bnode))
        (dist (make-distribution (discrete-bnode-arity bnode) 0.0d0)))
    (loop FOR p ACROSS ps FOR w ACROSS ws DO
          (increment-distribution dist (aref p i) w))
    (distribution->alist (normalize dist) bnode)))

(defmethod nil-summary ((bnode discrete-bnode))
  (distribution->alist NIL bnode :use-nils T))

(defmethod summarize-bnode ((bnode bnode) ps ws)
  ;; continuous bnodes:  xbar_w = weighted mean; s^2_w = weighted variance
  (unless (= (length ps) (length ws))
    (error "ps and ws vectors are of unequal length"))
  (let ((i (bnode-index bnode))
        (s^2_w 0.0d0)
        (xbar_w 0.0d0)
        (wsum (loop FOR w ACROSS ws SUM w))
        (nonzero-w-count (loop FOR w ACROSS ws UNLESS (zerop w) SUM 1)))
    (setf xbar_w (/ (loop FOR p ACROSS ps FOR w ACROSS ws 
                          SUM (* w (aref p i))) 
                    wsum))
    ;;CE added check for (< 1 nonzero-w-count) to prevent a divide by zero error
    ;;This is to catch the case where only one sample has a weight.
    (If (< 1 nonzero-w-count)
        (setf s^2_w (/ (loop FOR p ACROSS ps FOR w ACROSS ws
                             SUM (*  w  (expt (- (aref p i) xbar_w) 2)))
                       (/ (* (-- nonzero-w-count) wsum)
                          nonzero-w-count))))
    (list xbar_w (sqrt s^2_w))))


(defmethod nil-summary ((bnode bnode))
  (list NIL NIL))

(defun weights-sorted-by-value (bnode ps ws)
  "returns a list of lists, one sublist per particle.  Each sublist is (value weight), where
   the value is the value of bnode in that particle, and the sublists are ordered by value."
  (let* ((bnode-i (bnode-index bnode))
         (new-list (loop FOR p ACROSS ps FOR w ACROSS ws COLLECT (list (aref p bnode-i) w))))
    (sort new-list #'(lambda (a b) (< (first a) (first b))))))

(defun percentile-query (query ps ws)
  (let* ((vws (weights-sorted-by-value (first query) ps ws))
         (cum-wt (second (car vws))))
    (loop FOR percentile IN (rest query) COLLECT
          (progn
            (loop UNTIL (or (null (cdr vws)) (>= cum-wt percentile)) DO
                  (setf vws (cdr vws))
                  (incf cum-wt (second (car vws))))
            (caar vws)))))

(defun distribution-query (query ps ws)
  ;; recall the query structure for our only type, :uniform : (bnode :uniform min max bincount)
  (destructuring-bind (bnode dist-type min max bincount) query
                      (let* ((vws (weights-sorted-by-value bnode ps ws))
                             (result (loop FOR limit = min THEN (+ limit (/ (- max min) bincount))
                                           REPEAT (1+ bincount)
                                           COLLECTING (let ((bin-weight 0.0d0))
                                                        (loop FOR (val wt) = (car vws)
                                                              UNTIL (or (null (car vws)) (>= val limit))
                                                              DO 
                                                              (setf vws (cdr vws))
                                                              (incf bin-weight wt))
                                                        bin-weight))))
                        (append result
                                (list (+ 0.0d0 (loop FOR lastnodes IN vws SUM (second lastnodes))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PF-* FUNCTIONS USE SHARED VARIABLES DEFINED HERE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let (dbn
      query-vars
      predicate-queries
      percentile-queries
      distribution-queries
      ps ; particles: each is an evidence vector for the entire DBN (both slices)
      ws ; weights 
      use-slice0
      cumu-dist ; cumulative representation of weights distribution
      cumu-buckets ; indexes [0,1], in N equal intervals, into cumu-dist for O(1) resampling
      ;; describe above better
      bucket-size
      nodes-per-slice)
  
  (defun pf-init (net &key (N 1000) (queries T) (predicate-query-list NIL)
                      (percentile-query-list NIL) (distribution-query-list NIL))
    ;; ** predicate-query-list should be a list of predicate functions that are each applied to 
    ;; each particle after weighting.  For each function, for each step, the total weight
    ;; of the particles whose result is T is collected and reported.
    ;; ** percentile-query-list is a list of (nodename %ile %ile %ile) -- what will be reported
    ;; at each timestep is the nodename and the min value of nodename at which the accumulated
    ;; weight is >= each %ile. Or ... there may be better defs, but this is it for now.
    ;; ** distribution-query-list is a list of (nodename distribution arguments), for which the
    ;; only value of distribution now allowed is :uniform. For :uniform the arguments are
    ;; minval maxval bincount and the return is (nodename weight-below-min weight-at-least-max
    ;; [weight of values >= min+n*(max-min)/bincount and < next threshold] x n)
    
    ;; set up shared variables
    (setf dbn net)
    (when (eq queries T) (setf queries (dbn-var-names dbn)))
    (setf query-vars (mapcar (lambda (n) (or (bnode-by-name (subscript n 1) dbn)
                                             (error "query-vars: No node ~A here" n))) 
                             queries))
    (setf predicate-queries predicate-query-list)
    (setf percentile-queries
          (mapcar (lambda (query) ; check percentile-query-list as well as we can
                    (let* ((bnode-name (first query))
                           (bnode (bnode-by-name (subscript bnode-name 1) dbn)))
                      (unless bnode
                        (error "percentile-queries: No node ~A here" bnode-name))
                      (when (discrete-bnode-p bnode)
                        (error "percentile-queries: node ~A is discrete" bnode-name))
                      (mapl #'(lambda (l) (unless (or (< (length l) 2)
                                                      (< (first l) (second l)))
                                            (error "percentile-queries: ~A: percentiles unordered"
                                                   bnode-name)))
                            (rest query))
                      (mapc #'(lambda (v) (unless (and (< 0 v) (<= v 100))
                                            (error "percentile-queries: ~A: need 0<percentile<=100"
                                                   bnode-name)))
                            (rest query))
                      ;; OK, add to list. Change bnode ref to name, and convert % to decimal
                      (cons bnode (mapcar #'(lambda (p) (/ p 100.0d0)) (rest query)))))
                  percentile-query-list))
    (setf distribution-queries
          (mapcar (lambda (query) ; check distribution-query-list as well as we can
                    (let* ((bnode-name (first query))
                           (bnode (bnode-by-name (subscript bnode-name 1) dbn))
                           (dist-name (second query))
                           (args (rest (rest query))))
                      (unless bnode
                        (error "distribution-queries: No node ~A here" bnode-name))
                      (when (discrete-bnode-p bnode)
                        (error "distribution-queries: node ~A is discrete" bnode-name))
                      (case dist-name
                            (:uniform 
                              (unless (< (first args) (second args))
                                (error "distribution-queries: ~A: min>max" bnode-name))
                              (unless (and (integerp (third args)) (< 0 (third args)))
                                (error "distribution-queries: ~A: need natural bincount" bnode-name)))
                            (otherwise (error "distribution-queries: unknown dist ~A" dist-name)))
                      ;; OK.  Add to list but with bnode ref rather than name
                      (cons bnode (rest query))))
                  distribution-query-list))
    (setf nodes-per-slice (length (dbn-slice0 dbn)))
    (setf ps (make-sequence 'vector N))
    (loop FOR i FROM 0 TO (-- N) DO
          (setf (aref ps i) (make-sequence 'vector (* 2 nodes-per-slice))))
    (setf ws (make-sequence 'vector N :initial-element 0.0d0))
    (setf cumu-dist (make-sequence 'vector N :initial-element 0.0d0))
    (setf cumu-buckets (make-sequence 'vector N :initial-element 0))
    (setf bucket-size (/ 1 N))
    (setf use-slice0 T))
  ;; should I return a list of "headers" for the stats pf-step will return?
  
  
  (defun pf-resample ()
    ;; O(n) using SJR's O(1) sampling strategy
    ;; first build cumu-dist and cumu-buckets -- this step is O(n)
    
    (loop FOR i FROM 0 TO (-- (length ws))
          FOR s = (aref ws 0) THEN (+ s (aref ws i))
          DO (setf (aref cumu-dist i) s))
    (setf (aref cumu-dist (-- (length ws))) 1) ;; ensure the last entry is >= 1
    (let ((cdi 0)) ;; cdi is cumu-dist-index
      (loop FOR bucket-min FROM 0 TO (- 1 bucket-size) BY bucket-size
            FOR cbi UPFROM 0 DO ;; cbi is cumu-bucket-index
            (loop UNTIL (> (aref cumu-dist cdi) bucket-min) DO (incf cdi))
            (setf (aref cumu-buckets cbi) cdi)))
    ;; now we can sample in constant time per particle (on average)
    (loop FOR p ACROSS ps FOR r = (random 1.0d0) 
          FOR cbi = (floor (/ r bucket-size))
          FOR start-cdi = (aref cumu-buckets cbi)
          FOR cdi = (loop FOR i UPFROM start-cdi WHEN (>= (aref cumu-dist i) r) RETURN i)
          ;;Select the sample
          DO              
          (loop FOR i FROM 0 TO (-- nodes-per-slice) DO
                (setf (aref p i) (aref (aref ps cdi) (+ i nodes-per-slice))))
          
          )  
    )
  
  
  
  (defun pf-step (slice-evidence sliceno)
    "Filter one timestep using evidence provided as a list (w/ one value or NIL per node)
     -- return list of statistics as requested in pf-init"
    ;;(print sliceno)
    (let (nodes first-particle-index)
      (if use-slice0 
          (progn ; at the first step or the first after a reset, use slice0 nodes 
            (if (> sliceno 0)
                (format t "PF RESET - using slice 0~%"))
            (setf nodes (dbn-slice0 dbn)
                  first-particle-index 0)
            )
          
          (progn ; otherwise, use slice1; also need to resample from prior step's results
            (setf nodes (dbn-slice1 dbn)
                  first-particle-index nodes-per-slice)
            
            (pf-resample) 
            )
          )
      
      ;; now go through each node in each particle, sampling or weighting based on evidence
      (loop FOR p ACROSS ps AND wi UPFROM 0 DO
            ;;(format T "~%")
            (setf (aref ws wi) 1.0d0)
            (loop FOR node IN nodes AND node-value IN slice-evidence 
                  AND i UPFROM first-particle-index DO
                  ;;(format T "~A " (bnode-name node))
                  ;;(print (bnode-name node) )
                  (if (null node-value)
                      (setf (aref p i) (sample node p )) 	           
                      (setf (aref p i) node-value
                            (aref ws wi) (* (aref ws wi) 
                                            (conditional-probability node node-value p))
                            ))
                  #| (progn
                        (setf (aref p i) node-value)
                        (if (eq (bnode-index node) 29) ;;ce index of evidence node for ode 2
                            (setf (aref ws wi) (* (aref ws wi) 
                                            (conditional-probability node node-value p)))
                            )
                      )
                      )|#
                  
                   ;; (print (aref p i))
                   ;;(print (aref ws wi))
                  ))
      ;; final slice0 special case: move results to RHS of particles to ease future processing
      ;; CE For generating samples for EM
      #|(let* ((myname (concatenate 'string 
      "F:/My Documents/Cath/Lisp/aima/aima/CathLearning/InputData/SampleFile" 
      (format nil "~A" sliceno) ".csv")))
      (with-open-file (s myname :direction :output :if-does-not-exist :create :if-exists :overwrite)
      (loop FOR v IN query-vars DO
      (let ((i (bnode-index v)))
      (loop FOR p ACROSS ps DO
      (prin1 (aref p i) s)
      (fresh-line s))
      
      ))))|#
      ;;(print ps)
      ;;(print ws)
      
      (when use-slice0
        (setf use-slice0 NIL)
        (loop FOR p ACROSS ps DO
              (loop FOR i FROM 0 TO (-- nodes-per-slice) DO
                    (setf (aref p (+ i nodes-per-slice)) 
                          (aref p i)))))
      
      ;; Add step to validate all particles ce April 2010
      ;;(validateparticle ps ws use-slice0 dbn)

      ;; collect summary stats
      (multiple-value-bind (ws-ptr all-weights-zero) (dnormalize ws t)
        (setf use-slice0 all-weights-zero) ; reset DBN on next step if no particles worked here
        (let
          ((basic (loop FOR v IN query-vars 
                        COLLECT (if all-weights-zero
                                    (nil-summary v)
                                    (summarize-bnode v ps ws))))
           (predicate (loop FOR fun IN predicate-queries 
                            COLLECT (if all-weights-zero
                                        NIL
                                        (loop FOR p ACROSS ps AND w ACROSS ws
                                              WHEN (funcall fun p) SUM w))))
           (percentile (loop FOR query IN percentile-queries 
                             COLLECT (percentile-query query ps ws)))
           (distribution (loop FOR query IN distribution-queries 
                               COLLECT (distribution-query query ps ws))))
          (list basic predicate percentile distribution))
        
        ;; CE for testing purposes only   
        #|       (let* ((myname (concatenate 'string 
        "F:/My Documents/Cath/Lisp/aima/Insulin/Outputs/Means/hnewmean" 
        (format nil "~A" sliceno) ".csv")))
        (with-open-file (s myname :direction :output :if-does-not-exist :create :if-exists :overwrite)
        (loop FOR v IN query-vars DO
        (let ((i (bnode-index v)))
        (loop FOR p ACROSS ps DO
        (prin1 (aref p i) s)
        (princ " " s))
        (fresh-line s)
        (loop FOR w ACROSS ws DO
        (prin1 w  s)
        (princ " " s))))))|#
        ;; CE for testing purposes only   
        #|   (if (OR (= sliceno 1) (= sliceno 0) (= sliceno 50) (= sliceno 100) (= sliceno 120)(= sliceno 121) (= sliceno 150) (= sliceno 200))
        (progn 
        (let* ((myname (concatenate 'string 
        "F:/My Documents/Cath/Lisp/aima/Insulin/Outputs/Means/P1Orig" 
        (format nil "~A" sliceno) ".csv")))
        (with-open-file (s myname :direction :output :if-does-not-exist :create :if-exists :overwrite)
        (loop FOR v IN query-vars DO
        (let ((i (bnode-index v)))
        (loop FOR p ACROSS ps DO
        (prin1 (aref p i) s)
        (fresh-line s))
        
        ))))))|#
        
        )))
  
  (defun pf-substep (nodelist sub-step dbn)
       (loop for j from 1 TO sub-step  DO
          ;; Loop through each particle
          (loop FOR p ACROSS ps DO
                ;; First put a copy of the particles in t+1 into t
                (loop FOR i FROM 0 TO (-- nodes-per-slice) DO
                      (setf (aref p i) (aref p (+ i nodes-per-slice)))
                      )
                ;; now sample each node in the sub step list 
                (loop FOR node IN nodelist DO            
                        (setf (aref p (bnode-index node)) (sample node p )) 
                       ; )                  	           
                      )
                )
          )
    )


) ;end of LET wrapping up the shared variables


;; make event lists without worrying about order of nodes in DBN
(defun evlist-by-name (dbn nodenames nodevalues)
  "Given a DBN, node names, and their evidence values, returns an evidence list for pf-step"
  (unless (= (length nodenames) (length nodevalues))
    (error "need to supply one value per node name"))
  (loop FOR node IN (dbn-var-names dbn)
        FOR pos = (position node nodenames)
        COLLECT
        (if pos
            (nth pos nodevalues)
            NIL)))



;; compatibility wrapper for functions using SJR's particle-filter-by-name
(defun particle-filter-by-name (queries evidence dbn &key (N 1000) (percentile-query-list NIL) (substep-list NIL) (sub-step 0))
  (let ((var-count (length (dbn-var-names dbn))))
    (pf-init dbn :N N :queries queries :percentile-query-list percentile-query-list)
    (list 'FILLER 'FILLER 
          (loop FOR i FROM 0 TO (- (length evidence) var-count) BY var-count 
                FOR slice-evidence = (loop FOR j FROM 0 TO (-- var-count) 
                                           COLLECT (aref evidence (+ i j)))
                COLLECT (pf-step slice-evidence (/ i var-count))
                DO (pf-substep substep-list sub-step dbn)
                )
          )
    )
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
                                (loop for trial FROM 1 to num-trials do
                                      (setf totals (mapcar #'+ totals
                                                           (mapcar #'rms-error-enumerated 
                                                                   exact 
                                                                   (funcall approx-by-name Xname e dbn N)))))
                                (mapcar #'(lambda (x) (/ x (1+ k))) totals)))
                      (concatenate 'string file-prefix (format nil "~A" N) ".data")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun csv-from-null-ev (model steps filename)
  "run one particle 'free' through the DBN 'model' for 'steps' timesteps -- output as CSV into filename."
  (pf-init model :N 1)
  (with-open-file (STR filename :direction :output :if-exists :supersede 
                       :if-does-not-exist :create)
                  (format STR "STEP~{,\"~,4F\"~}~%" (dbn-var-names model))
                  (loop FOR step FROM 1 TO steps DO
                        (format STR "~D~{,~,4F~}~%" step
                                (mapcar #'car (car (pf-step (evlist-by-name model NIL NIL) step)))))))

