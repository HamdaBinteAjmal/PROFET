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
;;;; Created on 2008-11-17 20:41:35
(defun test-neo-dbn()
  (setq neo-dbn (load-bn "F:/My Documents/Cath/Lisp/aima/Neo/pkneo5V7.dbn"))
  
  ;;(dot-dbn neo-dbn "F:/My Documents/Cath/Lisp/aima/Neo/Outputs/pkneo5dots")
   ;;(dbn-var-names neo-dbn)
  
  (let* ((dataset experimental-data)
         (no-of-events (length dataset))
         (vars-per-slice 17)
         ;;(no-of-slices (1+ (elt (elt dataset (1- no-of-events)) 0))))
         (no-of-slices 800))

   
"produce an evidence vector based on the given data"

    (setq e1 (let* ((output (make-sequence 'vector (* no-of-slices vars-per-slice) :initial-element NIL)))
               (loop FOR i FROM 0 TO (1- no-of-events) DO
                     (setf (aref output (+ 6 (* (elt (elt dataset i) 0) vars-per-slice))) (elt (elt dataset i) 1))
                     (setf (aref output (+ 9 (* (elt (elt dataset i) 0) vars-per-slice))) (elt (elt dataset i) 2))
                     (setf (aref output (+ 13 (* (elt (elt dataset i) 0) vars-per-slice))) (elt (elt dataset i) 3))
                     (setf (aref output (+ 16 (* (elt (elt dataset i) 0) vars-per-slice))) (elt (elt dataset i) 4)))
               output))

    (time (let* ((nodelist (list 'true-serum-conc 'true-qty-urine 'k-12 'k-21 'k-elim 'serum-vol 'recorded-quantity))
                 (output (particle-filter-by-name nodelist e1 neo-dbn1 :N 1000)))
            (with-open-file (s "F:/My Documents/Cath/Lisp/aima/Neo/Outputs/datamean.csv" :direction :output :if-does-not-exist :create 
                               :if-exists :overwrite)
                            (LOOP FOR i FROM 0 TO (1- (length (third output))) DO 
                                  (prin1 i s)
                                  (princ " " s)
                                  (LOOP FOR j FROM 0 TO (1- (length nodelist)) DO
                                        ;;mean and standard deviation
                                        (prin1 (first (elt (first (first (subseq (third output) i (+ 1 i)))) j)) s)
                                        (princ " " s)
                                        (prin1 (second (elt (first (first (subseq (third output) i (+ 1 i)))) j)) s)
                                        (princ " " s)
                                        )
                                  (fresh-line s)
                                  )
                            
)))))

(defun test-neo-pkpd-dbn()
  (setq neo-dbn1 (load-bn "F:/My Documents/Cath/Lisp/aima/Neo/PDModel/pkpdneo5.dbn"))
  
  ;;(dot-dbn neo-dbn "F:/My Documents/Cath/Lisp/aima/Neo/Outputs/pkneo5dots")
   ;;(dbn-var-names neo-dbn)
  
  (let* ((dataset experimental-data)
         (no-of-events (length dataset))
         (vars-per-slice 25)
         ;;(no-of-slices (1+ (elt (elt dataset (1- no-of-events)) 0))))
         (no-of-slices 20))

   
"produce an evidence vector based on the given data"

    (setq e1 (let* ((output (make-sequence 'vector (* no-of-slices vars-per-slice) :initial-element NIL)))
               (loop FOR i FROM 0 TO (1- no-of-events) DO
                     (setf (aref output (+ 2 (* (elt (elt dataset i) 0) vars-per-slice))) (elt (elt dataset i) 1))
                     (setf (aref output (+ 13 (* (elt (elt dataset i) 0) vars-per-slice))) (elt (elt dataset i) 2))
                     (setf (aref output (+ 20 (* (elt (elt dataset i) 0) vars-per-slice))) (elt (elt dataset i) 3))
                     (setf (aref output (+ 23 (* (elt (elt dataset i) 0) vars-per-slice))) (elt (elt dataset i) 4))
                     (setf (aref output (+ 24 (* (elt (elt dataset i) 0) vars-per-slice))) (elt (elt dataset i) 5))
                     (setf (aref output (+ 12 (* (elt (elt dataset i) 0) vars-per-slice))) (elt (elt dataset i) 6)))
               output))

   ;; (time
           (let* ((nodelist (list 'Emax 'EC50 'Hill-factor 'delta-effect 'true-serum-conc))
                 (output (particle-filter-by-name nodelist e1 neo-dbn1 :N 1000)))
            ( print output)
             
         #|   (with-open-file (s "F:/My Documents/Cath/Lisp/aima/Neo/Outputs/pkpddata.csv" :direction :output :if-does-not-exist :create 
                               :if-exists :overwrite)
                            (LOOP FOR i FROM 0 TO (1- (length (third output))) DO 
                                  (prin1 i s)
                                  (princ " " s)
                                  (LOOP FOR j FROM 0 TO (1- (length nodelist)) DO
                                        ;;mean and standard deviation
                                        (prin1 (first (elt (first (first (subseq (third output) i (+ 1 i)))) j)) s)
                                        (princ " " s)
                                        (prin1 (second (elt (first (first (subseq (third output) i (+ 1 i)))) j)) s)
                                        (princ " " s)
                                        )
                                  (fresh-line s)
                                  )
                            
)|#
        ;;    )
    )))


  