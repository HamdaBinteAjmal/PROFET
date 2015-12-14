/* 
 * Copyright (C) 2010 Alex Billingsley, email@alexbillingsley.co.uk
 * www.dragmath.bham.ac.uk 
 * This file is part of DragMath.
 * This file has been modified by Hamda Binte Ajmal to be added
 * to the project PROFET.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */



 
 

package EquationEditor.Tree;

/**
 * Class representing a Matrix, that has an n x n array of <code>MathObject</code> objects.
 * @author Alex Billingsley
 */
public class Matrix extends MathObject {
    
    private MathObject[][] array;
    private int m;
    private int n;
    
    /** Creates a new instance of Matrix
     * Initialises the fields from the parameters
     */
    public Matrix(int id, String name, int m, int n) {
        super(id, name);
        this.m=m;
        this.n=n;
        array = new MathObject[m][n];
        array[0][0] = null;
    }
    
    /** Sets an element in the array at the location from the parameters
     * @param m the int to specify which row to add the element in
     * @param n the int to specify which column to add the element in
     * @param element the <code>MathObject</code> to add at the location [m][n]
     */
    public void setElement(int m, int n, MathObject element) {
        array[m][n] = element;
    }
    
    /** Returns the array
     * @return The field <code>array</code> of type <code>MathObject[][]</code>
     */
    public MathObject[][] getArray() {
        return array;
    }
    
    /** Returns the element in the array at the location from the parameters
     * @param m the int to specify which row to get the element from
     * @param n the int to specify which column to get the element from
     * @return the element of type <code>MathObject</code> from the location in the array specified by the parameters
     */
    public MathObject getElement(int m, int n) {
        return array[m][n];
    }
    
    /** Returns the number of rows in the array
     * @return An int for the number of rows
     */
    public int getM() {
        return m;
    }
    
    /** Returns the number of columns in the array
     * @return An int for the number of columns
     */
    public int getN() {
        return n;
    }
}
