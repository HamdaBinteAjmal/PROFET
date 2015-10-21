Starting a new DBN?
-------------------
norm 2008-10-02


Load aima.lisp and (aima-load 'probability)

(help-edit-nets) will show you the basic commands.

(defparameter *NEW* (make-dbn))
(loop :for name :in '(rain umbrella) :do (add-node *NEW* name :discrete)
... and so on until the net seems ready to run.  

dot-dbn makes a dot file (use :tooltips NIL if you don't have all
functions done).  Show it with
 $ dot -Tpdf new.dot -o new.pdf
 $ open new.pdf

Now you're ready to try running it.

Code I used on the BPA model is in~/icu/papers/bpa-papers/uai-2008/

The main functions for running it are pf-init and pf-step

(At this time, pf-init and pf-step have a lot of statistics-collecting
stuff in them that should be outside.)



