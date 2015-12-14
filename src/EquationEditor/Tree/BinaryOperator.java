/* 
 * Copyright (C) 2010 Alex Billingsley, email@alexbillingsley.co.uk
 * www.dragmath.bham.ac.uk 
 * This file is part of DragMath.
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
 * Class representing a Binary Operator, that has two arguments, <code>leftChild</code> and <code>rightChild</code>,
 * which are of type <code>MathObject</code>.
 * @author Alex Billingsley
 */
public class BinaryOperator extends MathObject {
    
    private MathObject leftChild = null;
    private MathObject rightChild = null;
    
    
    /** Creates a new instance of BinaryOperator */
    public BinaryOperator(int id, String name) {
        super(id, name);
    }
    
    /** Returns the field <code>leftChild</code>
     * @return The field <code>leftChild</code> of type <code>MathObject</code>
     */
    public MathObject getLeftChild() {
        return leftChild;
    }
    
   /** Returns the field <code>rightChild</code>
     * @return The field <code>rightChild</code> of type <code>MathObject</code>
     */
    public MathObject getRightChild() {
        return rightChild;
    }
    
    /** Sets the field <code>leftChild</code> from the parameter
     * @param leftChild The <code>MathObject</code> to set as the <code>leftChild</code>
     */
    public void setLeftChild(MathObject leftChild) {
        this.leftChild = leftChild;
    }
    
    /** Sets the field <code>rightChild</code> from the parameter
     * @param rightChild The <code>MathObject</code> to set as the <code>rightChild</code>
     */
    public void setRightChild(MathObject rightChild) {
        this.rightChild = rightChild;
    }

    
}