;;; File: ansi-cl.lisp -*- Mode: Lisp; Syntax: Common-Lisp; -*-

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

 
