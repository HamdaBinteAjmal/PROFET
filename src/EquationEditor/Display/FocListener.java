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
import javax.swing.border.*;
import java.awt.event.*;
import java.util.Stack;
import EquationEditor.Tree.BuildTree;
import EquationEditor.Tree.MathObject;

/**
 * Class to control border round empty/non-empty text boxes and selected boxes
 * also splits text boxes into other objects when possible
 * @author Alex Billingsley
 */
public class FocListener implements FocusListener {
    
    private AddComponent addComponent;
    private String oldContent;
    
    /** Creates a new instance of FocListener */
    public FocListener() {
    }
    
    /** Creates a new instance of FocListener */
    public FocListener(AddComponent addComponent) {
        this.addComponent=addComponent;
    }
    
    public void setAddComponent(AddComponent addComponent) {
        this.addComponent=addComponent;
    }
    
    // Puts border round box because it has focus
    public void focusGained(FocusEvent evt) {
        JTextField target = (JTextField)evt.getSource();
        target.setBorder(new EtchedBorder());
        oldContent = target.getText();
    }
    
    // Removes border from box, unless it is empty, tries to parse string into tree
    public void focusLost(FocusEvent evt) {
        JTextField target = (JTextField)evt.getSource();
        if (target.getText().length() > 0) {
            target.setBorder(new EmptyBorder(target.getInsets()));
        }
        
        // Saves state if contents of box have changed
        if (oldContent.equals(target.getText())) {
            
        } else {
            String newContent = target.getText();
            target.setText(oldContent);
            addComponent.saveState(true);
            target.setText(newContent);
        }
        
        Stack boxStack;
        try {
            boxStack = BuildTree.parseString(target.getText(), new Stack());
            int i=0;
            Stack outputStack = new Stack();
            while (i < boxStack.size()) {
                outputStack.push(boxStack.get(i));
                i++;
            }
            if (outputStack.size() > 1) {
                MathObject tree = (MathObject)outputStack.pop();
                BuildTree.toTree(tree, outputStack);
                
                JPanel parent = (JPanel)target.getParent();
                int j = addComponent.getComponentPosition(parent, target);
                parent.remove(target);
                addComponent.pasteTree(parent, j, tree, 0);
            }
        } catch (org.nfunk.jep.ParseException ex) {
            // No error message given, because if string cannot be parsed into tree, then it is left as a string in the text box
            // so will be treated as a Text object in the tree
        }
    }
}
