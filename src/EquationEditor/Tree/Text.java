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
 * Class to represent a Text object, consisting of a string of characters
 * @author Alex Billingsley
 */
public class Text extends MathObject {
    
    private String text;
    
    /** Creates a new instance of Text 
     * Initialises the parameters
     */
    public Text(String text) {
        super(-1, "");
        this.text = text;
    }

    /** Returns the string of text
     * @return The string of text
     */
    public String getText() {
        return text;
    }
    

}
