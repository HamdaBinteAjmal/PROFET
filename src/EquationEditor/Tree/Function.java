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
 * Class representing a Function, that has one argument, <code>child</code>,
 * which is of type <code>MathObject</code>.
 * @author Alex Billingsley
 */
public class Function extends MathObject {
    
    private MathObject child = null;
    
    /** Creates a new instance of Function */
    public Function(int id, String name) {
        super(id, name);
    }
    
    /** Returns the field <code>child</code>
     * @return The field <code>leftChild</code> of type <code>MathObject</code>
     */
    public MathObject getChild() {
        return child;
    }
    
    /** Sets the field <code>child</code> from the parameter
     * @param child the <code>MathObject</code> to set as the <code>child</code>
     */
    public void setChild(MathObject child) {
        this.child=child;
    }
    
    
    
}