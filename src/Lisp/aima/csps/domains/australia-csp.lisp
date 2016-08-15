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

;;;; australia-csp.lisp    

;;; The Australia map-coloring CSP [2e p 138].
;;; The following definition uses the enumerated-constraint type, 
;;; in which the constraint is defined by a set of allowed tuples. 
;;; One could also use an neq-constraint, as defined in nqueens-csp.lisp 

(defvar *australia-csp*)
(setf *australia-csp* 
      (make-csp
       :variables '(WA NT SA Q NSW V TA)
       :domains '((WA . (red green blue))
		  (NT . (red green blue))
		  (SA . (red green blue))
		  (Q . (red green blue))
		  (NSW . (red green blue))
		  (V . (red green blue))
		  (TA . (red green blue)))
       :constraints `(,(make-enumerated-constraint :variables '(WA NT)
						   :tuples '((red green) (red blue) (green red) (green blue) (blue red) (blue green)))
		      ,(make-enumerated-constraint :variables '(WA SA)
						   :tuples '((red green) (red blue) (green red) (green blue) (blue red) (blue green)))
		      ,(make-enumerated-constraint :variables '(NT SA)
						   :tuples '((red green) (red blue) (green red) (green blue) (blue red) (blue green)))
		      ,(make-enumerated-constraint :variables '(NT Q)
						   :tuples '((red green) (red blue) (green red) (green blue) (blue red) (blue green)))
		      ,(make-enumerated-constraint :variables '(SA Q)
						   :tuples '((red green) (red blue) (green red) (green blue) (blue red) (blue green)))
		      ,(make-enumerated-constraint :variables '(SA NSW)
						   :tuples '((red green) (red blue) (green red) (green blue) (blue red) (blue green)))
		      ,(make-enumerated-constraint :variables '(SA V)
						   :tuples '((red green) (red blue) (green red) (green blue) (blue red) (blue green)))
		      ,(make-enumerated-constraint :variables '(Q NSW)
						   :tuples '((red green) (red blue) (green red) (green blue) (blue red) (blue green)))
		      ,(make-enumerated-constraint :variables '(NSW V)
						   :tuples '((red green) (red blue) (green red) (green blue) (blue red) (blue green)))
		      )))

