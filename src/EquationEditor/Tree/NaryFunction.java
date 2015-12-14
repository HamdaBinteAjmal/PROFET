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
 * Class representing an NaryFunction, that has two n-arguments
 * which are of type <code>MathObject</code>.
 * @author Alex Billingsley
 */
public class NaryFunction extends MathObject {
    
    private MathObject[] child;
    
    
    /** Creates a new instance of NaryFunction */
    public NaryFunction(int id, String name, int n) {
        super(id, name);
        child = new MathObject[n];
    }
    
     public MathObject getChild(int n) {
        return child[n];
    }

    public int getSize() {
        return child.length;
    }
     
 
    public void setChild(MathObject newChild, int n) {
        child[n] = newChild;
    }
    
    
}