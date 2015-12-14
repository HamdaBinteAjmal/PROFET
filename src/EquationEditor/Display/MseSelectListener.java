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

import java.awt.event.*;
import java.awt.Point;
import javax.swing.*;
import java.awt.*;

/**
 * Class to store mouse start location when dragging,
 * and to deselect any selected components when the mouse is pressed
 * @author Alex Billingsley
 */
public class MseSelectListener extends MouseAdapter {
    
    private Point xy1;
    public static final Color DESELECT = new Color(Color.WHITE.getRGB());
    public static final Color SELECT = new Color(Color.LIGHT_GRAY.getRGB());
    
    /** Creates a new instance of MouseSelectionListener */
    public MseSelectListener() {
    }
    
    public void mousePressed(MouseEvent e) {
        xy1 = new Point(e.getX(), e.getY());
        JPanel layer = (JPanel)e.getComponent();
        deSelect(layer);
    }
    
    
    public static void deSelect(JPanel layer) {
        
        Component[] components = layer.getComponents();
        int i=0;
        while (i < components.length) {
            
            if (components[i].getClass().getName().equals("javax.swing.JLabel")) {
                if (components[i].getBackground().equals(SELECT)) {
                    JLabel temp = (JLabel)components[i];
                    temp.setOpaque(false);
                    components[i].setBackground(DESELECT);
                }
            }
            
            if (components[i].getClass().getName().equals("EquationEditor.Display.TextBox")) {
                if (components[i].getBackground().equals(SELECT)) {
                    components[i].setBackground(DESELECT);
                }
            }
            
            // If panel is selected, de-select all components nested on panel
            if (components[i].getClass().getName().equals("javax.swing.JPanel")) {
                if (components[i].getBackground().equals(SELECT)) {
                    components[i].setBackground(DESELECT);
                }
                deSelect((JPanel)components[i]);
            }
            
            i++;
        }
    }
    
    /**
     * Getter for property xy1.
     * @return Value of property xy1.
     */
    public Point getXy1() {
        return this.xy1;
    }
    
}
