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
import java.awt.event.*;
import java.awt.*;

/**
 * Class to control the re-sizing of graphics in the display, as other components re-size
 * @author Alex Billingsley
 */
public class ComListener extends ComponentAdapter {
    
    private int status;
    private JPanel layer;
    private int firstPos;
    private int secondPos;
    
    /** Creates a new instance of ComListener
     * @param layer the JPanel to watch if it resizes
     * @param status the int to state what type of components are to be resized
     */
    public ComListener(JPanel layer, int status, int firstPos, int secondPos) {
        this.status = status;
        this.layer=layer;
        this.firstPos=firstPos;
        this.secondPos=secondPos;
        //resize();
    }
    
    public void componentResized(ComponentEvent e) {
        resize();
    }
    
    public void resize() {
        int i=0;
        int height=0;
        java.awt.FontMetrics fontMetrics=null;
        JPanel parentLayer = (JPanel)layer.getParent();
        
        // Determine what size font is required
        while (height < layer.getPreferredSize().getHeight()) {
            i++;
            fontMetrics  = layer.getFontMetrics(new java.awt.Font("SansSerif", 0, i));
            height = fontMetrics.getHeight();
        }
        
        // Brackets
        if (status == 0) {
            JLabel bracket1 = (JLabel)parentLayer.getComponent(firstPos);
            JLabel bracket2 = (JLabel)parentLayer.getComponent(secondPos);
            bracket1.setFont(new java.awt.Font("SansSerif", 0, i));
            bracket2.setFont(new java.awt.Font("SansSerif", 0, i));
        }
        // Symbol for a function e.g. square root
        if (status == 1) {
            JLabel symbol = (JLabel)parentLayer.getComponent(0);
            symbol.setFont(new java.awt.Font("SansSerif", 0, i));
        }
        // Symbol for a function e.g. definite integral
        if (status == 2) {
            JPanel temp = (JPanel)parentLayer.getComponent(0);
            JLabel symbol = (JLabel)temp.getComponent(1);
            symbol.setFont(new java.awt.Font("SansSerif", 0, i));
        }
        
        
        parentLayer.revalidate();
    }
}
