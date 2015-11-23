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

;;;  -*- Mode: Lisp; Syntax: Common-Lisp -*- File: aima.lisp

;;;; Vendor-Specific Customizations

#+Lucid (setq *warn-if-no-in-package* nil)

#+SBCL (declaim (optimize (debug 2)) )



;;;; A minimal facility for defining systems of files

(defparameter *aima-root* (truename *load-truename*)
  "The root directory where the code is stored.")

(defparameter *aima-binary-type*
  (third (list   ; <<<<<<<<<<<<<<<<<<<< Edit this <<<<<<<<<
	   #+Lispworks system::*binary-file-type*
	   #+Lucid (first lucid::*load-binary-pathname-types*) 
           #+Allegro excl:*fasl-default-type*
	   #+(or AKCL KCL) "o"
	   #+CMU "sparcf"
	   #+CLISP "fas"))
  "If calling aima-load loads your source files and not your compiled
  binary files, insert the file type for your binaries before the <<<<
  and load systems with (aima-load-binary NAME).")

(defconstant *aima-version*
  "1.90 AIMA2e Code, first rough draft")

(defparameter *aima-system-names* nil
  "A list of names of the systems that have been defined.")

(defstruct aima-system
  name (requires nil) (doc "") (parts nil) (examples nil) (loaded? nil))

;;;; The Top-Level Functions:

(defmacro def-aima-system (name requires doc &body parts)
  "Define a system as a list of parts.  A part can be a string, which denotes
  a file name; or a symbol, which denotes a (sub)system name; or a list of the
  form (subdirectory / part...), which means the parts are in a subdirectory.
  The REQUIRES argument is a list of systems that must be loaded before this 
  one.  Note that a documentation string is mandatory."
  `(add-aima-system :name ',name
		    :requires ',requires :doc ',doc :parts ',parts))

(defun aima-load (&optional (name 'all))
  "Load file(s), trying the system-dependent method first."
  (operate-on-aima-system name 'load-something))

(defun aima-load-binary (&optional (name 'all))
  "Load file(s), prefering binaries to source."
  (operate-on-aima-system name 'load-binary))			  

(defun aima-compile (&optional (name 'everything))
  "Compile (and load) the file or files that make up an AIMA system."
  (operate-on-aima-system name 'compile-load))

(defun aima-load-if-unloaded (name)
  (let ((system (get-aima-system name)))
    (unless (and system (aima-system-loaded? system))
      (aima-load system))
    system))

;;;; Support Functions

(defun add-aima-system (&key name requires doc parts examples)
  (pushnew name *aima-system-names*)
  (setf (get 'aima-system name)
	(make-aima-system :name name :examples examples
			  :requires requires :doc doc :parts parts)))

(defun get-aima-system (name)
  "Return the system with this name.  (If argument is a system, return it.)"
  (cond ((aima-system-p name) name)
	((symbolp name) (get 'aima-system name))
	(t nil)))

(defun operate-on-aima-system (part operation &key (path nil) (load t)
				    (directory-operation #'identity))
  "Perform the operation on the part (or system) and its subparts (if any).
  Reasonable operations are load, load-binary, compile-load, and echo.
  If LOAD is true, then load any required systems that are unloaded."
  (let (system)
    (cond
     ((stringp part) (funcall operation (aima-file part :path path)))
     ((and (consp part) (eq (second part) '/))
      (let* ((subdirectory (mklist (first part)))
	     (new-path (append path subdirectory)))
	(funcall directory-operation new-path)
	(dolist (subpart (nthcdr 2 part))
	  (operate-on-aima-system subpart operation :load load 
				  :path new-path
				  :directory-operation directory-operation))))
     ((consp part)
      (dolist (subpart part)
	(operate-on-aima-system subpart operation :load load :path path
				:directory-operation directory-operation)))
     ((setf system (get-aima-system part))
      ;; Load the required systems, then operate on the parts
      (when load (mapc #'aima-load-if-unloaded (aima-system-requires system))) 
      (operate-on-aima-system (aima-system-parts system) operation
			      :load load :path path
			      :directory-operation directory-operation)
      (setf (aima-system-loaded? system) t))
     (t (warn "Unrecognized part: ~S in path ~A" part path)))))

(defun aima-file (name &key (type nil) (path nil))
  "Given a file name and maybe a file type and a relative path from the 
  AIMA directory, return the right complete pathname."
  (make-pathname :name name :type type :defaults *aima-root*
		 :directory (append (pathname-directory *aima-root*)
				    (mklist path))))

#-(or Allegro MCL) ;; Macintosh CL has this function; Allegro needs warning muffler
(defun compile-load (file)
  "Compile file and then load it."
  ;; This could be made more sophisticated, to compile only when out of date.
  (compile-file (file-with-type file "lisp"))
  (load-binary file))

#+Allegro
(defun compile-load (file)
  "Compile file and then load it."
  ;; This could be made more sophisticated, to compile only when out of date.
  (handler-bind ((excl:compiler-no-in-package-warning
		  #'(lambda (c) (declare (ignore c)) (muffle-warning))))
		(compile-file (file-with-type file "lisp")))
  (load-binary file))

(defun load-binary (file)
  "Load file, trying the binary first, but loading the source if necessary."
  (load-something file '(binary nil "lisp")))

(defun load-something (file &optional (types '(nil binary "lisp")))
  "Try each of the types in turn until we get a file that loads.
  Complain if we can't find anything.  By default, try the system-dependent
  method first, then the binary, and finally the source (lisp) file."
  (dolist (type types (warn "Can't find file: ~A" file))
    (when (load (file-with-type file type) :if-does-not-exist nil)
      (return t))))

(defun file-with-type (file type)
  "Return a pathname with the given type."
  (if (null type)
      file
    (merge-pathnames
     (make-pathname :type (if (eq type 'binary) *aima-binary-type* type))
     file)))

(defun mklist (x)
  "If x is a list, return it; otherwise return a singleton list, (x)."
  (if (listp x) x (list x)))

;;; ----------------------------------------------------------------------
;;;; Definitions of Systems
;;; ----------------------------------------------------------------------

(def-aima-system utilities ()
  "Basic functions that are loaded every time, and used by many other systems."
  ("utilities" / "ansi-cl" "utilities" "queue" "array" 
   "probability" "statistics" "test-utilities"))

(def-aima-system agents (utilities)
  "Code from Part I: Agents and Environments"
  ("agents" / "test-agents"
   ("domains" / )
   ("algorithms" / )
   ("agents" / "agent" "vacuum-agents")
   ("environments" / "basic-env" "vacuum-env")))

(def-aima-system search (agents)
  "Code from Chapters 3 and 4: Solving Problems by Searching; Informed Search and Exploration"
  ("search" / "test-search" 
   ("domains" / "problems" "enumerated-problem" "path-planning"
                           "sliding-block-puzzle" "route-finding" "traveling-salesperson-problem" "vacuum-problem" 
                           "nqueens-problem" "maze" "random-walk-trap-problem" "lrta-demo-problem"
		"poproblems")
   ("algorithms" / "problem-solving" "tree-search" "graph-search" "local-search")
   ("agents" / "simple-problem-solving-agent" "online-search")
   ("environments" / "problem-env" "poproblem-env")))

(def-aima-system csps (agents search)
  "Code from Chapter 5: Constraint Satisfaction Problems"
  ("csps" / "test-csps" 
   ("domains" / "csps" "australia-csp" "nqueens-csp")
   ("algorithms" / "backtracking-search" "constraint-propagation" "min-conflicts")
   ("agents" / )
   ("environments" / )))

(def-aima-system probability (agents)
  "Code from Chapters 13, 14: Probability, Bayes nets."
  ("probability" / "test-probability"
		 ;; edit-nets added below by norm 2007-06-04
   ("domains" / "bayes-nets" "dbns" "edit-nets")
   ("algorithms" / "bn-exact-inference" "bn-approx-inference" "dbn-inference")
   ("environments" / )
   ("agents" / )))

(def-aima-system mdps (agents probability)
  "Code from Chapters 16, 17: Decision networks and MDPs."
  ("mdps" / "test-mdps"
   ("domains" / "mdps" "4x3-mdp")
   ("algorithms" / "dp" "expectimax")
   ("agents" / "simple-mdp-solving-agent" "expectimax-agent")
   ("environments" / "mdp-env")))


(def-aima-system all ()
  "All systems except the utilities system, which is always already loaded"
  agents search csps probability mdps)

(def-aima-system everything ()
  "All the code, including the utilities"
  utilities all)

(setf *aima-system-names* (nreverse *aima-system-names*))

;;;; Always load the utilities

(aima-load 'utilities)
