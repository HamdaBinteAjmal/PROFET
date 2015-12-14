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

import java.io.File;
import javax.swing.*;
import javax.swing.filechooser.*;

/**
 *
 * @author Alex Billingsley
 */
public class DrgmFileFilter extends FileFilter {
    
    
    /** Creates a new instance of DrgmFileFilter */
    public DrgmFileFilter() {
    }
    
    
    public boolean accept(File f) {
        if(f != null) {
            if (f.isDirectory()) return true;
            String filename = f.getName();
            int i = filename.lastIndexOf('.');
            if(i > 0 && i < filename.length() - 1) {
                if (filename.substring(i+1).toLowerCase().equals("drgm")) return true;
            }
        }
        return false;
    }
    
    public static boolean isDrgmFile(File f) {
        if(f != null) {
            String filename = f.getName();
            int i = filename.lastIndexOf('.');
            if(i > 0 && i < filename.length() - 1) {
                if (filename.substring(i+1).toLowerCase().equals("drgm")) return true;
                if (f.isDirectory()) return true;
            }
        }
        return false;
    }
    
    public String getDescription(){
        return "DragMath expression (*.drgm)";
    }
}
