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

;; Created by Catherine Enright on 2011-08-31 10:12:13
;; The purpose of this module is to provide an interface for running particle-filtering on a dbn
;;
;; The function run-inference has 4 mandatory parameters and 2 optional ones.
;;       dbn-filename:      The filename (including path) of the dbn specification
;;       evidence-filename: The filename (including path) of the evidence. 
;;                          The first row must contain the nodenames_0 separated by spaces and end with "nil"
;;                          After the first row there should be one row per time step containing the evidence 
;;                          values again separated by spaces. If no evidence exists for a time step use "nil"
;;       output-filename:   The filename (including path) where the output should be sent. Note if the filename 
;;                          already exists it will be overwritten without warning.
;;       no-timestep:       The number of steps for which inference must be performed
;;
;;       no-samples:         Number of samples to use, defaults to 1000
;;
;; Change Log: Version 2: no-samples is now a mandatory parameter and return teh results for all nodes.
;; Change Log: Version 3: Fixed output file so that if a node contains more than 2 discrete states the results are output.
;;
(defun run-inference (dbn-filename evidence-filename output-filename no-timesteps 
                                   no-samples )
  
  (let* ((dbn (sort-dbn (load-bn dbn-filename )))
         (vars-per-slice (list-length (dbn-var-names dbn)))
         (evidence (make-sequence 'vector (* no-timesteps vars-per-slice) :initial-element NIL))
         (output)
         )
    
    ;; read in the evidence from the file to the vector
    (load-evidence evidence-filename dbn vars-per-slice evidence no-timesteps)
    
    ;;run particle filtering on the DBN
    (setq output (particle-filter-by-name (dbn-var-names dbn) evidence dbn :N no-samples))
    
    ;;write the output to a file
    (dbn-results-to-file output output-filename (dbn-var-names dbn))
    
    ))


(defun load-evidence (evidence-filename dbn  vars-per-slice evidence no-timesteps)
  
  (let ((node-indexes (make-array 0 :fill-pointer t :adjustable t))
        (temp (make-array 0 :fill-pointer t :adjustable t))
        )
    ;; populate the node-indexes array with the indexes of teh nodenames listed in the first line of the file
    (with-open-file (in evidence-filename :direction :input :if-does-not-exist :error)
                    (when in
                      (loop for value = (read in nil)
                            while value do 
                            (vector-push-extend (bnode-index (bnode-by-name value dbn)) node-indexes)
                            )
                      ;; Now load the evidence values into a temporary array
                      (loop for value = (read in nil 'foo)
                            until (eq value 'foo)
                            do (vector-push-extend value temp)
                            )
                      ))
    ;; Populate the evidence vector. We may have more or less evidence than 
    ;; than the number of timesteps for which we want to perform inference
    ;; the code should handle both situations.
    (loop FOR i FROM 0 TO (-  (min no-timesteps 
                                   (/ (length temp) (length node-indexes))) 1) 
          do
          (loop for j from 0 to (- (length node-indexes) 1) do
                (setf (aref evidence (+ (* vars-per-slice i ) (aref node-indexes j)) )
                      (aref temp (+ (* (length node-indexes) i ) j)))
                )
          )
    )
  )

(defun dbn-results-to-file (output output-filename nodelist )
  (print output)
  
  (with-open-file (s output-filename
                     :direction :output :if-does-not-exist :create :if-exists :SUPERSEDE)
                  (format s "TimeStep ~{~S  ~}" nodelist)
                  (fresh-line s)
                  (LOOP FOR i FROM 0 TO (1- (length (third output))) DO 
                        (prin1  i s) 
                        (princ " " s)
                        (LOOP FOR j FROM 0 TO (1- (length nodelist)) DO
                              ;;mean and standard deviation for continuous nodes, state and probability for discrete
                              (loop for k from 0 TO (1- (length (elt (first (first (subseq (third output) i (+ 1 i)))) j))) DO
                                    (format s "~15,7,,'*F" (elt (elt (first (first (subseq (third output) i (+ 1 i)))) j) k) )
                                    (princ " " s)
                                    )
                              )
                        (fresh-line s)
                        ))
  
  )
