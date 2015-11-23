;;; 
 ;; PROFET Copyright 2015 (c) Data Mining and Machine Learning Group,
 ;; National University of Ireland Galway.  
 ;; This file is a part of PROFET  
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

;;; author : Hamda binte Ajmal
(defun create-model-coefficients(termnames dbnname nodetype)
  "Create nodes of type nodetype for each termname passed as a list 
   Each node will be conditionally dependant on its value in the previous slice"
  
  (dolist (termname termnames)
    (add-node dbnname termname nodetype) 
    (add-parents dbnname (subscript termname 1) (list (subscript termname 0)))
	) 
  )
  (defun create-true-value-node (nodenames dbnname)
  "Create deterministic node for nodename 
   The true value node will be determined based on the parents in the list
   These should be previous true value and previous delta node"
  (dolist (nodename nodenames)
    (let ((deltaname (intern (concatenate 'string "delta" (symbol-name nodename))))
          )
      
      ;; Create true node and delta node
      (add-node dbnname nodename :deterministic)
      (add-node dbnname deltaname :deterministic)
      
      ;; Make the true value dependant on the true value in the previous slice and the delta previous slice
	  (print (subscript nodename 1))
	  (print (subscript nodename 0))
	  (print deltaname)
      (add-parents dbnname (subscript nodename 1) (list (subscript nodename 0)))
      (add-parents dbnname (subscript nodename 1) (list (subscript deltaname 0)))
      
      ;; Make the delta a child of variable(true value) in the current time slice
      ;;(add-parents dbnname deltaname (list  nodename )) ;; need to figure it out with catherine
	  (print deltaname)
     
      )
    ) 
  )
  ;; ask catherine about last 2 lines
(defun create-evidence-nodes (nodename parentname dbnname)
  ;;(dolist (nodename nodenames)
    ;;(let ((observedname (intern (concatenate 'string "Observed-" (symbol-name nodename)))))
      (add-node dbnname nodename :linear-gaussian) 
      (add-parents dbnname nodename parentname) 
	  ;;(add-parents icing-dbn 'Observed-BG '(BG))
      ;;(add-parents dbnname (subscript nodename 0) (list (subscript observedname 0)))
      ;;(add-parents dbnname (subscript nodename 1) (list (subscript observedname 1)))
      ;;))
  )
(defun create-model-inputs (nodenames dbnname)
  (dolist (nodename nodenames)
    (let ((observedname (intern (concatenate 'string "Intended-" (symbol-name nodename)))))
      (add-node dbnname observedname :linear-gaussian) 
      (add-node dbnname nodename :linear-gaussian)  
      
      (add-parents dbnname (subscript nodename 0) (list (subscript observedname 0)))
      (add-parents dbnname (subscript nodename 1) (list (subscript observedname 1)))
      ))
  )
  (defun create-truncated-gaussian-parameter-cpts (dbn parm mean std lowerlimit upperlimit parentstd)
  
  ;; Sort of a hack cos I know my variable nodes have no parents
  (setf (truncated-gaussian-bnode-coefficients-table  (bnode-by-name (subscript parm 0) dbn)) 
        (my-make-array nil :initial-element nil))
  (setf (truncated-gaussian-bnode-offset-table  (bnode-by-name (subscript parm 0) dbn)) 
        (my-make-array nil :initial-element mean))
  (setf (truncated-gaussian-bnode-sigma-table  (bnode-by-name (subscript parm 0) dbn)) 
        (my-make-array nil :initial-element std))
  (setf ( truncated-gaussian-bnode-lower-limit-table  (bnode-by-name (subscript parm 0) dbn)) 
        (my-make-array nil :initial-element lowerlimit))
  (setf ( truncated-gaussian-bnode-upper-limit-table  (bnode-by-name (subscript parm 0) dbn)) 
        (my-make-array nil :initial-element upperlimit))
  
  (setf (truncated-gaussian-bnode-coefficients-table  (bnode-by-name (subscript parm 1) dbn)) 
        (my-make-array nil :initial-element (list 1))) ;; hardcoded value, might change later
  (setf (truncated-gaussian-bnode-offset-table  (bnode-by-name (subscript parm 1) dbn)) 
        (my-make-array nil :initial-element 0))
  (setf (truncated-gaussian-bnode-sigma-table  (bnode-by-name (subscript parm 1) dbn)) 
        (my-make-array nil :initial-element parentStd));; change this
  (setf ( truncated-gaussian-bnode-lower-limit-table  (bnode-by-name (subscript parm 1) dbn)) 
        (my-make-array nil :initial-element lowerlimit))
  (setf ( truncated-gaussian-bnode-upper-limit-table  (bnode-by-name (subscript parm 1) dbn)) 
        (my-make-array nil :initial-element upperlimit))
  
  )
  
   (defun linear-truncated-gaussian-parameter-cpts (dbn parm mean std parentstd)
  
  ;; Sort of a hack cos I know my variable nodes have no parents
  (setf (linear-gaussian-bnode-coefficients-table  (bnode-by-name (subscript parm 0) dbn)) 
        (my-make-array nil :initial-element nil))
  (setf (linear-gaussian-bnode-offset-table  (bnode-by-name (subscript parm 0) dbn)) 
        (my-make-array nil :initial-element mean))
  (setf (linear-gaussian-bnode-sigma-table  (bnode-by-name (subscript parm 0) dbn)) 
        (my-make-array nil :initial-element std))
  
  (setf (linear-gaussian-bnode-coefficients-table  (bnode-by-name (subscript parm 1) dbn)) 
        (my-make-array nil :initial-element (list 1))) ;; hardcoded value, might change later
  (setf (linear-gaussian-bnode-offset-table  (bnode-by-name (subscript parm 1) dbn)) 
        (my-make-array nil :initial-element 0))
  (setf (linear-gaussian-bnode-sigma-table  (bnode-by-name (subscript parm 1) dbn)) 
        (my-make-array nil :initial-element parentStd))
  
  )
  
  (defun create-uniform-parameter-cpts (dbn parm a b)
  
  ;; Sort of a hack cos I know my variable nodes have no parents
  (setf (uniform-bnode-a-lower-table  (bnode-by-name (subscript parm 0) dbn)) 
        (my-make-array nil :initial-element a))
  (setf (uniform-bnode-b-upper-table  (bnode-by-name (subscript parm 0) dbn)) 
        (my-make-array nil :initial-element b))
  (setf (uniform-bnode-a-lower-table (bnode-by-name (subscript parm 1) dbn)) 
        (my-make-array nil :initial-element a))
  (setf (uniform-bnode-b-upper-table  (bnode-by-name (subscript parm 1) dbn)) 
        (my-make-array nil :initial-element b))
  
  )
(defun create-evidence-cpts  (dbn parm std parentCoeff parentOffset)
 
    (loop for subs in '(0 1) DO
          (setf (linear-gaussian-bnode-coefficients-table  (bnode-by-name (subscript parm subs) dbn)) 
                (my-make-array nil :initial-element (list parentCoeff)))
          (setf (linear-gaussian-bnode-offset-table  (bnode-by-name (subscript parm subs) dbn)) 
                (my-make-array nil :initial-element parentOffset))
          (setf (linear-gaussian-bnode-sigma-table  (bnode-by-name (subscript parm subs) dbn)) 
                (my-make-array nil :initial-element std))
          )
   
  )

(defun create-input-cpts  (dbn parm mean std parentStd parentCoeff parentOffset)
  (let ((observedname (intern (concatenate 'string "Intended-" (symbol-name parm))))
        )
    (loop for subs in '(0 1) DO
          (setf (linear-gaussian-bnode-coefficients-table  (bnode-by-name (subscript observedname subs) dbn)) 
                (my-make-array nil :initial-element nil))
          (setf (linear-gaussian-bnode-offset-table  (bnode-by-name (subscript observedname subs) dbn)) 
                (my-make-array nil :initial-element mean))
          (setf (linear-gaussian-bnode-sigma-table  (bnode-by-name (subscript observedname subs) dbn)) 
                (my-make-array nil :initial-element std))
          )
    (loop for subs in '(0 1) DO
          (setf (linear-gaussian-bnode-coefficients-table  (bnode-by-name (subscript parm subs) dbn)) 
                (my-make-array nil :initial-element (list parentCoeff))) ;; it depends on parent
          (setf (linear-gaussian-bnode-offset-table  (bnode-by-name (subscript parm subs) dbn)) 
                (my-make-array nil :initial-element parentOffset))
          (setf (linear-gaussian-bnode-sigma-table  (bnode-by-name (subscript parm subs) dbn)) 
                (my-make-array nil :initial-element parentStd))
          )
    )
  )
;;;; For run inference
(defun read-in-evidence-fixed (filename evidencename)
	
  
    
  (with-open-file (in filename 
                      :direction :input :if-does-not-exist :error)
                  (when in
                    (loop for value = (read in nil)
                          while value do 
                          (vector-push-extend value evidencename) ;;convert from icumm to icing units
                          )
                    )
                  )
				 
				  )
(defun add-parent-indexes (dbn)
  ;; For each node in the
  ;; Check if it has a parent
  ;; If so fill parent indexes
  
  (loop for node in (bn-nodes dbn) Do
        
        (loop for parent in (discrete-parents node) Do
              (push (bnode-index parent) (bnode-discrete-parent-indexes node))
              )
        (loop for parent in (continuous-parents node) Do
              (push (bnode-index parent) (bnode-continuous-parent-indexes node))
              ))
  ;;(save-bn testdbn "F:/My Documents/Cath/Lisp/aima/Insulin/test.dbn")
  
  )
  
  (defun File-line-count (inputstream)
  
  (do ((size 0 (1+ size))
       (line (read inputstream nil nil) ;;because my data is set up as a list I am using read instead of read-line
             (read inputstream nil nil)))
      ((null line) size))
  ) 
  
(defun read-in-continous-evidence-adaptive  (filename column nodename dbnname)
(print "1")
(with-open-file (in filename )
 (setf no-entries  (file-line-count in))
 (close in))
 
 (print "2")
  (with-open-file (in filename  
                        :direction :input :if-does-not-exist :error)
                    (let* ((i 0)
                           
                           (c-evidence (make-array (list no-entries 3)))
                           )
                      
                      
                      (when in
                        (loop for value = (read in nil)
                              while value do 
                              (setf (aref c-evidence i 0 ) (bnode-index (bnode-by-name nodename dbnname)))
							  (print "3")
                              (setf (aref c-evidence i 1 ) (first value))
							  (print "4")
                              (setf (aref c-evidence i 2 ) (second value))
							  (print "5")
                              ;; Insulin IV-status
                              
                              (setf i (+ 1 i))                             
                              )
                        )
						(close in)
                      (setf (aref continuous-evidence column) c-evidence)
                      
                      )
                    )
)
(defun read-in-instantaneous-evidence-adaptive  (filename column nodename dbnname)
(with-open-file (in filename )
 (setf no-entries  (file-line-count in))
 (close in))
(with-open-file (in filename  
                        :direction :input :if-does-not-exist :error)
                    (let* ((i 0)
                           
                           (i-evidence (make-array (list no-entries 3)))
                           )
                     
                      
                      (when in
                        (loop for value = (read in nil)
                              while value do 
                              (setf (aref i-evidence i 0 ) (bnode-index (bnode-by-name nodename dbnname)))
                              (setf (aref i-evidence i 1 ) (first value))
                              (setf (aref i-evidence i 2 ) (second value))
                              (setf i (+ 1 i))                             
                              )
                        )
						(close in)
                      (setf (aref instant-evidence column) i-evidence)
                      )
                    )
					)