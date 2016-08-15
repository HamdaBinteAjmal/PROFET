;;; array.lisp
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
;;; General code for operations on vectors, 2-D matrices, and
;;; arrays of arbitrary dimension. 

;;; Also includes some old code off the web for linear algebra on 2D matrices
;;; This code has some overlap with the array code; it hasn't been tested
;;; and needs to be integrated with the array code stylistically and
;;; terminologically.



;;; Mapping operations on arrays of arbitrary dimension

;;; mapindices is the primitive iterator over indices of arbitrary dimensions
;;; the function f takes a list of indices as its argument; it is called
;;; with all possible indices in the ranges specified by dimensions

(defun mapindices (f dimensions 
                   &optional (indices (make-list (length dimensions)))
                             (rest-indices indices))
  (if (null dimensions) (funcall f indices)
      (loop for i from 0 to (1- (first dimensions)) do
        (setf (first rest-indices) i)
        (mapindices f (rest dimensions) indices (rest rest-indices)))))
  

;;; mapindices-project iterates over all indices for dimensions
;;; but keeps some dimensions fixed at given values.
;;; f is called with two arguments: indices and projected indices (w/o fixed).

(defun mapindices-project (f dimensions fixed values)
  (let ((i (make-list (length dimensions)))
	(dimensions-fixed (copy-list dimensions))
	(p (make-list (- (length dimensions) (length fixed)))))
    (mapcar #'(lambda (dim value) 
		(setf (nth dim i) value)
		(setf (nth dim dimensions-fixed) nil))
	    fixed values)
    (mapindices-project2 f dimensions-fixed i i p p)))

(defun mapindices-project2 (f dimensions i rest-i p rest-p)
  (cond ((null dimensions) (funcall f i p))
	((null (first dimensions))
	 (mapindices-project2 f (rest dimensions) 
			      i (rest rest-i) p rest-p))
	(t (dotimes (j (first dimensions))
	     (setf (first rest-i) j)
	     (setf (first rest-p) j)
	     (mapindices-project2 f (rest dimensions) 
				  i (rest rest-i) p (rest rest-p))))))

;;; project-array returns a new copy of array with
;;; the dims dimensions reduced to fixed values

(defun project-array (array dims values)
  (let* ((dimensions (array-dimensions array))
	 (parray (my-make-array (remove-nths dims dimensions))))
    (mapindices-project #'(lambda (i p) (setf (apply #'aref parray p)
					      (apply #'aref array i)))
			dimensions dims values)
    parray))

;;; copy-array copies arrays of arbitrary dimension; elements are copied by
;;; the copier-function

(defun copy-array (m &optional (copier #'identity)
		     &aux (d (array-dimensions m))
			  (m2 (if (null d) (make-array nil) (make-array d))))
  (dmap-array #'(lambda (x2 x) (declare (ignore x2)) (funcall copier x)) m2 m))

;;; add-array adds arrays m1 and m2, returning the result

(defun add-array (m1 m2) (map-array #'+ m1 m2))

;;; dadd-array adds array m2 into m1, returning the modified m1

(defun dadd-array (m1 m2) (dmap-array #'+ m1 m2))

;;; scale-array scales array m by amount x, returning the result

(defun scale-array (x m) (map-array #'(lambda (y) (* x y)) m))

;;; dscale-array scales array m2 into m1, returning the modified m1

(defun dscale-array (x m) (dmap-array #'(lambda (y) (* x y)) m))

;;; map-array applies a given function to all elements of the
;;; array (or corresponding elements of all arrays if more than one given)
;;; and returns a new array containing the results

(defun map-array (f array &rest arrays)
  (apply #'dmap-array f (copy-array array) arrays))

;;; dmap-array applies a given function to all elements of the
;;; array (or corresponding elements of all arrays if more than one given)
;;; and stores the results back into the first array - which is returned.
;;; Probably not as efficient as it could be - might be better
;;; to use :displaced-to keyword option and map along a one-dimensional
;;; array instead of mapping over all dimensions.

(defun dmap-array (f array &rest arrays)
  (mapindices 
   #'(lambda (i)
       (setf (apply #'aref array i)
             (apply f (apply #'aref array i)
                      (mapcar #'(lambda (a) (apply #'aref a i)) arrays))))
   (array-dimensions array))
  array)

  

;;; my-make-array is identical to make-array except that it works 
;;; correctly when the dimensions are nil. (Allegro CL bug)

(defun my-make-array (dims &key (element-type t) (initial-element nil))
  (if (null dims) 
      (if (typep initial-element element-type) 
	  (make-array nil :element-type element-type :initial-element initial-element)
	(make-array nil :element-type element-type))
    (if (typep initial-element element-type) 
	(make-array dims :element-type element-type :initial-element initial-element)
      (make-array dims :element-type element-type))))


(defun array->vector (array)
  "Convert a multi-dimensional array to a vector with the same elements."
  (make-array (array-total-size array) :displaced-to array))


(defun array*array (a b &aux (ra (array-dimension a 0))
		             (ca (array-dimension a 1))
		             (cb (array-dimension b 1))
		             (r (make-array (list ra cb))))
  (dotimes (row ra r)
    (dotimes (col cb)
      (setf (aref r row col)
	    (let ((sum 0))
	      (dotimes (i ca sum) (incf sum (* (aref a row i) (aref b i col)))))))))

(defun array-array-product (a b f1 f2
                      &aux (ra (array-dimension a 0))
		           (cb (array-dimension b 1))
		           (r (make-array (list ra cb))))
  (dotimes (i ra r)
    (dotimes (j cb)
      (setf (aref r i j)
	    (reduce f1 (map 'list f2 (get-row a i) (get-col b j)))))))

(defun array-vector-product (a x f1 f2
                      &aux (ra (array-dimension a 0))
		           (r (make-sequence 'vector ra)))
  (dotimes (i ra r)
      (setf (aref r i)
	    (reduce f1 (map 'list f2 (get-row a i) x)))))


;;; return a vector corresponding to the ith row of array a
(defun get-row (a i &aux (ca (array-dimension a 1))
			 (result (make-sequence 'vector ca)))
  (dotimes (j ca result) (setf (aref result j) (aref a i j))))

;;; return a vector corresponding to the jth column of array a
(defun get-col (a j &aux (ra (array-dimension a 0))
		         (result (make-sequence 'vector ra)))
  (dotimes (i ra result) (setf (aref result i) (aref a i j))))

(defun array*vector (a x &aux (ra (array-dimension a 0))
			      (ca (array-dimension a 1))
			      (r (copy-seq x)))
  (dotimes (row ra r)
    (setf (aref r row)
	    (let ((sum 0))
	      (dotimes (i ca sum) (incf sum (* (aref a row i) (aref x i))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standard operations on 2-dimensional square matrices

(defun matrixp (matrix)
  "Test whether the argument is a matrix"
  (and (arrayp matrix)
       (= (array-rank matrix) 2)))

(defun num-rows (matrix)
  "Return the number of rows of a matrix"
  (array-dimension matrix 0))

(defun num-cols (matrix)
  "Return the number of rows of a matrix"
  (array-dimension matrix 1))

(defun square-matrix? (matrix)
  "Is the matrix a square matrix?"
  (and (matrixp matrix)
       (= (num-rows matrix) (num-cols matrix))))
       
(defun make-matrix (rows &optional (cols rows))
  "Create a matrix filled with zeros.  If only one parameter is
  specified the matrix will be square."
  (make-array (list rows cols) :initial-element 0))

(defun make-identity-matrix (size)
  "Make an identity matrix of the specified size."
  (let ((matrix (make-array (list size size) :initial-element 0)))
    (dotimes (i size matrix)
      (setf (aref matrix i i) 1))))

(defun copy-matrix (matrix)
  "Return a copy of the matrix."
  (let* ((rows (num-rows matrix))
         (cols (num-cols matrix))
         (copy (make-array (list rows cols))))
    (dotimes (row rows copy)
      (dotimes (col cols)
        (setf (aref copy row col) (aref matrix row col))))))

(defun print-matrix (matrix &optional (destination t) (control-string "~20S"))
  "Print a matrix.  The optional control string indicates how each
  entry should be printed."
  (let ((rows (num-Rows matrix))
        (cols (num-Cols matrix)))
    (dotimes (row rows)
      (format destination "~%")
      (dotimes (col cols)
        (format destination control-string (aref matrix row col))))
    (format destination "~%")))

(defun transpose-matrix (matrix)
  "Transpose a matrix"
  (let* ((rows (num-rows matrix))
         (cols (num-cols matrix))
         (transpose (make-matrix cols rows)))
    (dotimes (row rows transpose)
      (dotimes (col cols)
        (setf (aref transpose col row)
              (aref matrix row col))))))

(defun multiply-matrix (&rest matrices)
  "Multiply matrices"
  (labels ((multiply-two (m1 m2)
             (let* ((rows1 (num-rows m1))
                    (cols1 (num-cols m1))
                    (cols2 (num-cols m2))
                    (result (make-matrix rows1 cols2)))
               (dotimes (row rows1 result)
                 (dotimes (col cols2)
                   (dotimes (i cols1)
                     (setf (aref result row col)
                           (+ (aref result row col)
                              (* (aref m1 row i)
                                 (aref m2 i col))))))))))
    (when matrices                      ; Empty arguments check
      (reduce #'multiply-two matrices))))

(defun add-matrix (&rest matrices)
  "Add matrices"
  (labels ((add-two (m1 m2)
             (let* ((rows (num-rows m1))
                    (cols (num-cols m1))
                    (result (make-matrix rows cols)))
               (dotimes (row rows result)
                 (dotimes (col cols)
                   (setf (aref result row col)
                         (+ (aref m1 row col)
                            (aref m2 row col))))))))
    (when matrices                      ; Empty arguments check
      (reduce #'add-two matrices))))

(defun subtract-matrix (&rest matrices)
  "Subtract matrices"
  (labels ((subtract-two (m1 m2)
             (let* ((rows (num-rows m1))
                    (cols (num-cols m1))
                    (result (make-matrix rows cols)))
               (dotimes (row rows result)
                 (dotimes (col cols)
                   (setf (aref result row col)
                         (- (aref m1 row col)
                            (aref m2 row col))))))))
    (when matrices                      ; Empty arguments check
      (reduce #'subtract-two matrices))))

(defun zerop-matrix (m)
  (let* ((rows (num-rows m))
	 (cols (num-cols m)))
    (dotimes (row rows t)
      (unless (dotimes (col cols t)
                (unless (zerop (aref m row col)) (return nil)))
	(return nil)))))

(defun invert-matrix (matrix &optional (destructive T))
  "Find the inverse of a matrix.  By default this operation is
  destructive.  If you want to preserve the original matrix, call this
  function with an argument of NIL to destructive."
  (let ((result (if destructive matrix (copy-matrix matrix)))
        (size (num-rows matrix))
        (temp 0))
    (dotimes (i size result)
      (setf temp (aref result i i))
      (dotimes (j size)
        (setf (aref result i j)
              (if (= i j)
                  (/ (aref result i j))
                  (/ (aref result i j) temp))))
      (dotimes (j size)
        (unless (= i j)
          (setf temp (aref result j i)
                (aref result j i) 0)
          (dotimes (k size)
            (setf (aref result j k)
                  (- (aref result j k)
                     (* temp (aref result i k))))))))))

(defun exchange-rows (matrix row-i row-j)
  "Exchange row-i and row-j of a matrix"
  (let ((cols (num-cols matrix)))
    (dotimes (col cols)
      (rotatef (aref matrix row-i col) (aref matrix row-j col)))))


;(defun eliminate-matrix (matrix rows cols)
;  "Gaussian elimination with partial pivoting.  "
;  ;; Evaluated for side effect.  A return value of :singular indicates the
;  ;; matrix is singular (an error).
;  (let ((max 0))
;    (loop for i below rows
;     do (setf max i)
;     do (loop for j from (1+ i) below rows
;         do (when (> (abs (aref matrix j i))
;                     (abs (aref matrix max i)))
;              (setf max j)))
;     do (when (zerop (aref matrix max i))
;          (return-from eliminate-matrix :singular)) ; error "Singular matrix"
;     do (loop for k from i below cols   ; Exchange rows
;         do (rotatef (aref matrix i k) (aref matrix max k)))
;     do (loop for j from (1+ i) below rows
;         do (loop for k from (1- cols) downto i
;             do (setf (aref matrix j k)
;                      (- (aref matrix j k)
;                         (* (aref matrix i k)
;                            (/ (aref matrix j i)
;                               (aref matrix i i)))))
;               )))
;    matrix))


;(defun substitute-matrix (matrix rows cols)
;  (let ((temp 0.0)
;        (x (make-array rows :initial-element 0)))
;    (loop for j from (1- rows) downto 0
;     do (setf temp 0.0)
;     do (loop for k from (1+ j) below rows
;         do (incf temp (* (aref matrix j k) (aref x k))))
;     do (setf (aref x j) (/ (- (aref matrix j (1- cols)) temp) 
;                            (aref matrix j j))))
;    x))

;(defun solve-matrix (matrix &optional (destructive T) print-soln)
;  "Solve a matrix using Gaussian elimination
;   Matrix must be N by N+1
;   Assume solution is stored as the N+1st column of the matrix"
;  (let ((rows (num-rows matrix))
;        (cols  (num-cols matrix))
;        (result (if destructive matrix (copy-matrix matrix))))
;    (unless (= (1+ rows) cols)
;      (error "Ill formed matrix"))      ; Cryptic error message
;    (cond ((eq :singular (eliminate-matrix result rows cols)))
;          (T (let ((soln (substitute-matrix result rows cols)))
;               (when print-soln
;                 (loop for i below rows
;                  do (format t "~% X~A = ~A" i (aref soln i))))
;               soln)))))

;(PROVIDE :MatrixOps)



(defun determinant (matrix &optional (row 0) (cols (iota (array-dimension matrix 0)))
			   &aux (sum 0) (sign 1))
  (cond ((null (rest cols))
	 (aref matrix row (first cols)))
	(t (loop for col in cols do
		 (incf sum (* sign 
			      (aref matrix row col)
			      (determinant matrix (1+ row) (remove col cols :test #'=))))
		 (setf sign (- sign)))
	   sum)))




(defun scale-matrix (x matrix &optional (destructive nil))
  "multiply matrix by scalar"
  (let* ((rows (num-rows matrix))
	 (cols (num-cols matrix))
	 (result (if destructive matrix (make-matrix rows cols))))
    (dotimes (row rows result)
      (dotimes (col cols)
        (setf (aref result row col)
	      (* x (aref matrix row col)))))))

(defun zero-matrix (matrix)
  "Set matrix to all zeroes in place"
  (let* ((rows (num-rows matrix))
	 (cols (num-cols matrix)))
    (dotimes (row rows matrix)
      (dotimes (col cols)
        (setf (aref matrix row col) 0.0d0)))))

(defun mapf-matrix (f matrix)
  "Apply f to each element of matrix in place"
  (let* ((rows (num-rows matrix))
	 (cols (num-cols matrix)))
    (dotimes (row rows matrix)
      (dotimes (col cols)
        (setf (aref matrix row col) (funcall f (aref matrix row col)))))))

(defun incf-matrix (matrix addend-matrix)
  "Increment matrix by addend-matrix in place"
  (let* ((rows (num-rows matrix))
	 (cols (num-cols matrix)))
    (dotimes (row rows matrix)
      (dotimes (col cols)
        (incf (aref matrix row col) (aref addend-matrix row col))))))
