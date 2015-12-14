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

import java.util.ArrayList;

/**
 * Class representing an N-ary Operator, that has 2<x<n arguments
 * @author Alex Billingsley
 */
public class NaryOperator extends MathObject {
    
    private ArrayList list = new ArrayList();
    
    /** Creates a new instance of n_aryOperator */
    public NaryOperator(int id, String name) {
        super(id, name);
    }
    
    /** Returns the number of children
     * @return an integer for the number of children
     */
    public int getSize() {
        return list.size();
    }
    
    /** Adds a child at the location n
     * @param child the <code>MathObject</code> to set as the <code>child</code>
     * @param n the int of the location to add it at
     */
    public void addChild(MathObject child, int n) {
        list.add(child);
    }
    
    /** Returns the child at the location n specified in the parameter
     * @param n int of the location of the child to return
     * @return The child of type <code>MathObject</code> at location n
     */
    public MathObject getChild(int n) {
        return (MathObject)list.get(n);
    }
}
