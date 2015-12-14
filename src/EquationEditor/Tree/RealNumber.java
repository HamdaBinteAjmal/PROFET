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
 * Class to represent a RealNumber object, consisting of a double
 * @author Alex Billingsley
 */
public class RealNumber extends MathObject {
    
    private double number;
    
    /** Creates a new instance of RealNumber
     * Initialises the parameters
     */
    public RealNumber(double number) {
        super(-1, "");
        this.number = number;
    }
    
    /** Returns the number
     * @return the number
     */
    public String getNumber(boolean keepAsDouble) 
    {
       String strNum = "";
        if (Double.compare(Math.floor(number), number) == 0 && keepAsDouble == false) {
            int intNum = (int)number;
            strNum = Integer.toString(intNum);
        } else {
            strNum = Double.toString(number);
        }

       return strNum;
    }
    
}