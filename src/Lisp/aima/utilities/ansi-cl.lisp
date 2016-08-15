;;; File: ansi-cl.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-
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
;;;; Compatibility package to enable older/noncompliant Lisps
;;;; to emulate those aspects of ANSI Common Lisp that they do not support.
;;;; Currently no additional definitions required for:
;;;;     Allegro
;;;;     CLISP
;;;; but this file is being kept just in case.

(eval-when (eval compile load)
  ;; Make it ok to place a function definition on a built-in LISP symbol.
  #+(or Allegro EXCL)
  (dolist (pkg-name '(excl common-lisp common-lisp-user aclwin))
    (let ((pkg (find-package pkg-name)))
      (when pkg (setf (excl:package-definition-lock pkg) nil)))))

(defmacro define-if-undefined (&rest definitions)
  "Use this to conditionally define functions, variables; or macros that
  may or may not be pre-defined in this Lisp.  This can be used to provide
  CLtL2 compatibility for older Lisps."
  `(progn
     ,@(mapcar #'(lambda (def)
		   (let ((name (second def)))
		     `(when (not (or (boundp ',name) (fboundp ',name)
				     (special-operator-p ',name)
				     (macro-function ',name)))
		       ,def)))
	       definitions)))

 
