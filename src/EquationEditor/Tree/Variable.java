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
 * Class to represent a Variable object, consisting of a single character
 * @author Alex Billingsley
 */
public class Variable extends MathObject {
    
    private char varName;
    
    /** Creates a new instance of Variable
     * Initialises the parameters
     */
    public Variable(char varName, String type) {
        super(-1, type);
        this.varName = varName;
    }
    
    /** Returns the variable name
     * @return the variable name
     */
    public char getVarName() {
        return varName;
    }
    
}