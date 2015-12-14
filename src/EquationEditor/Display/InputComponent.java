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

import java.awt.Cursor;
import java.awt.*;

/**
 *
 * @author Alex Billingsley
 */
public class InputComponent {
    
    private String tooltip;
    private Cursor cursor;
    private int ID;
    private int group;
    private String displayText;
    private String alternativeText;
    private boolean useAlternativeText;
    private String tag;
    private Image image;
    
    /** Creates a new instance of InputComponent */
    public InputComponent(int ID, int group, String displayText, String tooltip, Cursor cursor, String tag, Image image) {
        this.tooltip=tooltip;
        this.cursor=cursor;
        this.ID=ID;
        this.group=group;
        this.displayText=displayText;
        this.tag=tag;
        this.image=image;
    }
    
    
    public String getTooltip() {
        return tooltip;
    }
    
    public Image getImage() {
        return image;
    }
    
    public Cursor getCursor() {
        return cursor;
    }
    
    public int getID() {
        return ID;
    }
    
    public int getGroup() {
        return group;
    }
    
    public String getTag() {
        return tag;
    }
    
    public String getDisplayText() {
        return displayText;
    }
    
    
}
