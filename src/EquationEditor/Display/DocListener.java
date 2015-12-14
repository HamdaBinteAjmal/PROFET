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



 
 

package EquationEditor.Display;

import javax.swing.*;
import javax.swing.event.*;

/**
 * Class to listen for changes of the text inside a text box
 * @author Alex Billingsley
 */
public class DocListener implements DocumentListener {
    
    private JTextField target;
    
    /** Creates a new instance of DocListener
     * @param target the text box to listen for changes to and resize accordingly
     */
    public DocListener(JTextField target) {
        this.target=target;
    }
    
    public void changedUpdate(DocumentEvent e) {
    }
    
    public void removeUpdate(DocumentEvent e) {
        resize();
    }
    
    public void insertUpdate(DocumentEvent e) {
        resize();
    }
    
    public void resize() {
        if (target.getText().length() > 0) {
            target.setColumns(0);
        } else {
            target.setColumns(1);
        }
        if (target.getParent() != null) {
            JPanel layer = (JPanel)target.getParent();
            layer.revalidate();
        }
    }
    
}
