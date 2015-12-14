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
 * Class to store the precedence of each different math component
 * @author Alex Billingsley
 */
public class Precedence {
    
    
    public static final int NONE=0;
    public static final int ADD_SUB=1;
    public static final int MULT_DIV=2;
    public static final int EXPONENTS_ROOTS=3;
    
    public static int[] value = new int[100];
    static
    { int i=0;
      while (i < value.length) {
          value[i] = NONE;
          i++;
      }
      
      value[0] = MULT_DIV;
      value[1] = MULT_DIV;
      value[2] = ADD_SUB;
      value[3] = ADD_SUB;
      value[5] = EXPONENTS_ROOTS;
      value[6] = EXPONENTS_ROOTS;
      value[7] = EXPONENTS_ROOTS;
      value[8] = EXPONENTS_ROOTS;
      
    }

}
