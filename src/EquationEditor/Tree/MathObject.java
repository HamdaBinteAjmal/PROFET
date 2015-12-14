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
 * Superclass for all different Math objects of the tree to inherit fields/methods from
 * @author Alex Billingsley
 */
public class MathObject implements java.io.Serializable {
    
    private String name;
    private int id;
    private MathObject parent = null;
    
    /** Creates a new instance of MathObject */
    public MathObject(int id, String name) {
        this.name=name;
        this.id=id;
    }
    
    /** Returns the field <code>name</code>
     * @return The string <code>name</code>
     */
    public String getName() {
        return name;
    }
    
    public int getID() {
        return id;
    }
    
    /** Sets the field <code>parent</code> from the parameter
     * @param parent the <code>MathObject</code> to set as the <code>parent</code>
     */
    public void setParent(MathObject parent) {
        this.parent = parent;
    }
    
    /** Returns the field <code>parent</code>
     * @return The field <code>parent</code> of type <code>MathObject</code>
     */
    public MathObject getParent() {
        return parent;
    }
    
}
