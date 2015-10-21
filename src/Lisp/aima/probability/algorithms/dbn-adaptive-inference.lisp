;;;; Rewrite of particle filtering in dbn-inference.lisp
;;;; to incorporate adaptive steps as developed by Niall Madden and Nhan Anh Thai
;;;;
;;;; Catherine Enright 2011
;;;;
;;;; Most of the methods in dbn-inference are copied directly here
;;;; Only pf-step and particle-filter-by-name are rewritten
;;; 
;;;; New methods are called apf-step and adaptive-pf-by-name
;;;;
;;;; 08/03 CE Changed setting of step size so that last recommended step size is used after each summary step
;;;; Previously the summary interval was always used 
;;;;
;;;; 28/03 Contains code to capture stats for Niall
;;;; 29/03 Removed stats collection
;;;  pf-resample modified to map last steps list to new set of particles.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adaptive PARTICLE FILTERING i.e. PF with adaptive time steps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro -- (rest) 
  `(1- ,rest))


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
      nodes-per-slice    
      last-step
      )
  
  (defun adaptive-pf-by-name (queries dbn summary-interval finish-time 
                                      &key 
                                      (weight-nodes nil)
                                      (N 1000) 
                                      (percentile-query-list NIL) 
                                      (tolerance-node-list NIL)
                                      (tolerance NIL)
                                      (defined-step 1)
                                      )
									
    (let* ((var-count (length (dbn-var-names dbn)))
           (current-t 0)
           (next-summary-t 0)
           (slice-evidence (make-sequence 'vector var-count ))
           (results)
           (step-size)
           (new-stepsize)
           (particlesteps 0)
           )
		  
      ;; initialize shared variables
      (apf-init dbn :N N :queries queries :percentile-query-list percentile-query-list)
	
      (setf last-step (make-sequence 'vector N :initial-element summary-interval)) ;; do this here where I know summaary interval
	 
      ;;Run zero slice
      (setf results (append results (list (append (apf-zero-slice dbn) (list 0 )))))   
              
      ;;Run the through to finish time
      (loop while (< current-t finish-time) DO
            (apf-resample)
            (print "5")
            (setf next-summary-t (get-next-t current-t summary-interval))
            (if (> next-summary-t finish-time ) (setf next-summary-t finish-time)) 
            ;; Run each particle forward to next summary step
            (LOOP FOR p FROM 0 TO (- N 1) DO
                  (let* ((particle (aref ps p))
                         (current-p-time current-t)  ;;reset time for next particle
                         (rejected nil)
                         ;;(decreasedstep nil)
                         )
						
                    (if (= current-p-time 0)
                        (setf step-size (- next-summary-t current-p-time)) ;;First time make step size summary interval
                        ;;(setf step-size (min new-stepsize (- next-summary-t current-p-time)));; Every other time use last recommended step size
                        (setf step-size (min (aref last-step p)
                                             (- next-summary-t current-p-time)));; Every other time use last recommended step size
                        )
						
                    ;;Loop until we get to the summary step
                    (LOOP WHILE (> step-size 0) DO
                          (setf totalnosteps (1+ totalnosteps)) ;; for stats only
                          (setf particlesteps (1+ particlesteps)) ;; for stats only
                          ;;(setf (aref totalsteps p) (1+ (aref totalsteps p))) ;; for stats only
                          (setf slice-evidence (set-slice-evidence (+ current-p-time step-size) finish-time))
                          (setf particle (apf-step particle step-size slice-evidence defined-step)) ;;must put what is returned into particle array)
                          ;; Check tolerance to see if this step should be kept or rejected
                          (multiple-value-bind (isrejected curr-stepsize next-stepsize) (check-tolerance particle tolerance-node-list tolerance step-size)
                            (setq rejected isrejected)
                            (setq new-stepsize next-stepsize)
                            (setq step-size curr-stepsize)
                            ) 
                         
                          (LOOP WHILE rejected DO
                                (setf rejectcount (1+ rejectcount));; for stats only
                                ;;(print "rejected")
                                ;;(format t "New step size ~A " step-size)
                                
                                (setf slice-evidence (set-slice-evidence (+ current-p-time step-size) finish-time))
                                (setf particle (apf-step particle step-size slice-evidence defined-step)) ;;must put what is returned into particle array)
                                (multiple-value-bind (isrejected curr-stepsize next-stepsize) (check-tolerance particle tolerance-node-list tolerance step-size)
                                  (setq rejected isrejected)
                                  (setq new-stepsize next-stepsize)
                                  (setq step-size curr-stepsize)
                                  )
                                )
                          
                          ;; Get everything ready for next iteration
                          (setf current-p-time (+ current-p-time step-size)) ;; increment time by step we have just taken
                          
                          ;;(format t "Current Time ~A Step size ~A and Glucose is ~A~%" current-p-time step-size
                          ;;(aref particle 12))
                          
                          (if (> (+ current-p-time new-stepsize) next-summary-t) 
                              (setf step-size (- next-summary-t current-p-time)) ;; Don't want to go passed summary step
                              (setf step-size new-stepsize) ;; Next time use the increased stepsize returned by check-tolerance
                              )
                         
                          ;; Propagate the particle forward by putting copy of particle in t+1 into t
                          ;; ONLY when this is not the last step, in this case resampling will take care of it
                          ;; We need both slices for weighting.
                          (if (> step-size 0)   
                              (progn                            
                                (loop FOR i FROM 0 TO (-- nodes-per-slice) DO
                                      (setf (aref particle i) (aref particle (+ i nodes-per-slice)))
                                      )
                                (setf (aref last-step p) new-stepsize)) ;; We don't want the step size recommended by the very last step 
                                                                        ;; as this is probably too small as it was just to get us to summary step
                                                                        ;; Instead store the recommended step from teh second last step
                              )
                          ) 
                    
                    (setf (aref ps p) particle)
                    
                    
                    ) 
                  )
            
            ;; All particles should now have reached the next summary time
            ;; Weight them based on weighted node list 
            (weight-particles weight-nodes slice-evidence dbn) ;; need to somehow relate this to time as as weight depends 
            ;;on transition model which depends on time step!!
            
         ;;  (vector-push-extend (/ particlesteps N) avgsteps) ;; stats only
          ;;  (setf particlesteps 0) ;; for stats only
            
            (setf  results (append results (list (append (summarize-slice) (list next-summary-t))))) ;; to be compatilble with pf code need list of lists here
            
            (setf current-t next-summary-t) ;; propagate time
            
            
            )
      (list 'FILLER 'FILLER results) ;; to be compatilble with pf code
      )
    )
  
  (defun apf-init (net &key (N 1000) (queries T) (predicate-query-list NIL)
                       (percentile-query-list NIL) (distribution-query-list NIL))
    ;; ce This is cut and paste from dbn-inference. I haven't changed it
    
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
    (loop FOR i FROM 0 TO (- N 1) DO
          (setf (aref ps i) (make-sequence 'vector (* 2 nodes-per-slice))))
    (setf ws (make-sequence 'vector N :initial-element 0.0d0))
    (setf cumu-dist (make-sequence 'vector N :initial-element 0.0d0))
    (setf cumu-buckets (make-sequence 'vector N :initial-element 0))
    (setf bucket-size (/ 1 N))
    (setf use-slice0 T)
    )
  
  (defun apf-resample ()
    ;; O(n) using SJR's O(1) sampling strategy
    ;; first build cumu-dist and cumu-buckets -- this step is O(n)
    ;; Ce added mapping of last step list to resampled particles
    
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
    (let ((temp-last-step (make-sequence 'vector  (length ws) :initial-element 0.0d0))) ;;CE
      (loop FOR p ACROSS ps FOR r = (random 1.0d0) 
            FOR cbi = (floor (/ r bucket-size))
            FOR start-cdi = (aref cumu-buckets cbi)
            FOR cdi = (loop FOR i UPFROM start-cdi WHEN (>= (aref cumu-dist i) r) RETURN i)
            FOR step-index UPFROM 0 ;;CE
            ;;Select the sample
            DO              
            (loop FOR i FROM 0 TO (-- nodes-per-slice) DO
                  (setf (aref p i) (aref (aref ps cdi) (+ i nodes-per-slice))))
            (setf (aref temp-last-step step-index) (aref last-step cdi));;CE for adaptive time steps
            )
      (setf last-step temp-last-step) ;;CE for adaptive time steps
      ) 
    ) 
  
  
  
  (defun get-next-t (current-t summary-interval)
    ;; Next t is either the next summary step or if evidence occurs/changes before this
    ;; Next t is the time at while we have evidence
    (let ((new-t (+ current-t summary-interval))
          )
      
#|      (loop FOR node-evidence ACROSS continuous-evidence DO
            (DOTIMES (i  (array-dimension  node-evidence 0))
              (if (AND (> (aref node-evidence i 1) current-t) (< (aref node-evidence i 1) new-t) )
                  (setf new-t (aref node-evidence i 1))                 
                  )
              )
            )|#
      (loop FOR node-evidence ACROSS instant-evidence DO
            (DOTIMES (i  (array-dimension  node-evidence 0))
              (if (AND (> (aref node-evidence  i 1) current-t) (< (aref node-evidence  i 1) new-t) )
                  (setf new-t (aref node-evidence i 1))       
                  )
              )
            )
      new-t)
    )
  
  (defun set-slice-evidence (current-t finish-time)
    ;;Both the continuous and instant Evidence Arrays have the same structure
    ;; They are populated with Node Evidence arrays for each node that has evidence
    ;; Each Node Evidence Array has the format: Node-Index Time Value
    (let ((lslice-evidence (make-sequence 'vector nodes-per-slice :initial-element NIL))
          ;;(let ((slice-evidence (make-sequence 'vector 10 :initial-element NIL)) ;; for unit testing only
          (largest-t 0)
          )
      ;;First loop through continuous evidence
     (loop FOR node-evidence ACROSS continuous-evidence DO
            (DOTIMES (i  (array-dimension  node-evidence 0)) 
              (if (AND (<= (aref node-evidence i 1) current-t) (<= largest-t (aref node-evidence i 1))) 
                  (progn
                    (setf (aref lslice-evidence (aref node-evidence i 0)) (aref node-evidence i 2) )
                    (setf largest-t (aref node-evidence i 1)) ;; Just in case evidence is not ordered by time
                    )                         
                  )
              )
            (setf largest-t 0)
            )
      ;; Temp kludge for abstract model
      ;;(setf (aref lslice-evidence 2)  (exp current-t))
      
      ;;Now loop through instant evidence
      ;; for test to calculate RMSE I don't want to include the evidence at the finish time
      (if (< current-t finish-time )
          (loop FOR node-evidence ACROSS instant-evidence DO
                (DOTIMES (i  (array-dimension  node-evidence 0))
                  (if (= (aref node-evidence i 1) current-t) 
                      (setf (aref lslice-evidence (aref node-evidence i 0)) (aref node-evidence i 2) )  
                      
                      )
                  )
                )
          )
      
      lslice-evidence)
    )
  (defun apf-step (particle step-size slice-evidence defined-step)
    "Filter one timestep using evidence provided as a list (w/ one value or NIL per node)"   
    (let ((nodes (dbn-slice1 dbn))
          (first-particle-index nodes-per-slice)
          (scale-factor (sqrt (/ defined-step step-size))) ;;The dbn transition model is based on defined step
          )
      (setf deltastep step-size) ;; specifially for dbn odes
      (loop FOR node IN nodes AND node-value across slice-evidence 
            AND i UPFROM first-particle-index DO
            (if (null node-value)
                (setf (aref particle i) (sample node particle scale-factor)) ;; need to add "scaling factor" to Sample based on step size DBN definition for gaussian nodes	           
                (setf (aref particle i) node-value)
                
                ) ;; end if
            ) ;; end loop  
      )
    particle
    )
  (defun apf-zero-slice(dbn)
    (let ((nodes (dbn-slice0 dbn))
          (first-particle-index 0)
          (lslice-evidence))
    ;; Look up zero slice evidence
    (setf lslice-evidence (set-slice-evidence 0 1)) ;; 1 is just a dummy finish time
    
    (loop FOR p ACROSS ps AND wi UPFROM 0 DO
          ;;(format T "~%")
          (setf (aref ws wi) 1.0d0)
          (loop FOR node IN nodes AND node-value across lslice-evidence 
                AND i UPFROM first-particle-index DO
                (if (null node-value)
                    (setf (aref p i) (sample node p )) ;; No scaling needed as only populating sensor model           
                    (setf (aref p i) node-value
                          (aref ws wi) (* (aref ws wi) 
                                          (conditional-probability node node-value p))
                          ))
                )              
          )
    ;; Need to copy particles to slice 1 so reampling will work
    (setf use-slice0 NIL)
    (loop FOR p ACROSS ps DO
          (loop FOR i FROM 0 TO (-- nodes-per-slice) DO
                (setf (aref p (+ i nodes-per-slice)) 
                      (aref p i))))
    (summarize-slice)
      )
    )
  
  (defun weight-particles (weight-nodes slice-evidence dbn)
    ;; for weight-nodes list always give _1 node
    ;; This method is never called for zero slice
    ;; As the evidence nodes we are using, only have parents in the current slice we don't have to consider the step size
    
    (loop FOR p ACROSS ps AND wi UPFROM 0 DO
          ;;(format T "~%")
          (setf (aref ws wi) 1.0d0)
          (if (not (null weight-nodes))
              ;; If the weight list is populated only weight based on the nodes in the list
              (loop FOR node IN weight-nodes DO
                    (let ((node-value (aref slice-evidence (- (bnode-index node) (length (dbn-var-names dbn))))))
                      (if (not (null node-value))
                          (progn
                            (setf (aref ws wi) (* (aref ws wi) 
                                                  (conditional-probability node node-value p))))
                          )
                      )
                    )
              ;; If the weight list is not populated, weight based on any node with evidence
              (loop FOR node IN (dbn-slice1 dbn) DO
                    (let ((node-value (aref slice-evidence (- (bnode-index node) (length (dbn-var-names dbn))))))
                      (if (not (null node-value))
                          (progn
                            (setf (aref ws wi) (* (aref ws wi) 
                                                  (conditional-probability node node-value p))))
                          )
                      
                      )
                    
                    )
              )
          )
    
    )
  
  (defun summarize-slice()
    ;; collect summary stats
    (multiple-value-bind (ws-ptr all-weights-zero) (dnormalize ws t)
      
      (setf use-slice0 all-weights-zero) ; reset DBN on next step if no particles worked here
      (if all-weights-zero (print "Reset"))
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
      )
    )
  
   
  (defun check-tolerance (particle tolerance-node-list tolerance stepsize ) 
    "Check tolerance and return updated step size"
    ;;Based on Niall and Thai's Matlab code 12/10/10 AdaptiveEuler
    ;;tolerance-node-list must be node index in the zero time slice
    ;;Returns 3 things: T/F to indicated if the step size should be rejected
    ;; the step size to use for this step
    ;; the step size to use for the next step
    (let ((d2y)
          (SafetyFactor 0.9)
          (M1 1.5)
          (M2 0.9)
          ;; (nodes-per-slice 2) ;; for unit testing only should be removed after
          )
      (if (LOOP FOR node IN tolerance-node-list 
                ;; d2y = max(abs(f(t(Step+1),y(:,Step+1))-fn))/h
                MAXIMIZE (abs (/ (- (aref particle node) (aref particle (+ node nodes-per-slice))) stepsize)) into temp-d2y
                
                FINALLY (setq d2y temp-d2y) (return (< (/ (* d2y (expt stepsize 2) ) 2) tolerance))
                
                )
      
          (progn
            ;;(print d2y)
            (setf increasedcount (1+ increasedcount))
            ;;h = min(sqrt(2*Tol/d2y)*SafetyFactor, h*5);% Choose a new stepsize based on above approximation.    
            (values NIL stepsize (min (* (sqrt (* 2 (/ tolerance d2y))) SafetyFactor) (* stepsize M1))) ;;commented for testing
            
            
            )
          (progn 
      
            ;;h = min(h/2, sqrt(2*Tol/d2y)*SafetyFactor);% Choose a new h 
            
            (let ((newstep (min (* stepsize M2) (* (sqrt (/ (* 2 tolerance) d2y)) SafetyFactor))) 
                  )  
              (values T newstep (min (* (sqrt (* 2 (/ tolerance d2y))) SafetyFactor) (* newstep M1))) 
               
              )
            )
          )
      )
    )
  
  
  ) ;end of LET wrapping up the shared variables