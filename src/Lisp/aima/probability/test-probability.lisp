;;; test-probability.lisp
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
(deftest probability
  "Test code for Bayes nets, temporal models."
  ((setq burglary-bn (load-bn "probability/domains/burglary.bn")))
  ((setq e (alist->event '((JohnCalls . true) (MaryCalls . true)) burglary-bn)))
  ((enumeration-ask-by-name 'Burglary e burglary-bn) (< 0.28416 (cdr (assoc 'true *)) 0.28418))
  ((elimination-ask-by-name 'Burglary e burglary-bn) (< 0.28416 (cdr (assoc 'true *)) 0.28418))
  ((rejection-sampling-ask-by-name 'Burglary e burglary-bn 100000) (< 0.2 (cdr (assoc 'true *)) 0.4))
  ((likelihood-weighting-ask-by-name 'Burglary e burglary-bn 100000) (< 0.2 (cdr (assoc 'true *)) 0.4))
  ((mcmc-ask-by-name 'Burglary e burglary-bn 100000) (< 0.2 (cdr (assoc 'true *)) 0.4))
  )


