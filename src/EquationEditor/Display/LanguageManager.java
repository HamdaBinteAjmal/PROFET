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

import org.jdom.*;
import org.jdom.input.*;
import java.net.URL;
import javax.swing.*;
import java.io.*;

/**
 *
 * @author Alex Billingsley
 */
public class LanguageManager {
    
    private Document languageFile;
    private Element lang;
    private StatusBar statusBar;
    private URL appletCodeBase;
    private SAXBuilder builder;
    
    /** Creates a new instance of Language */
    public LanguageManager(URL appletCodeBase, StatusBar statusBar) {
        this.appletCodeBase = appletCodeBase;
        this.statusBar=statusBar;
        builder = new SAXBuilder();
    }
    
    public String readLangFile(String childName) {
        Element child = null;
        if (lang != null) {
            child = lang.getChild(childName);
        }
        String text="";
        if (child != null) text = child.getText();
        else statusBar.println("Error: Missing data in language file");
        return text;
    }
    
    
    public void loadLanguageFile(String language) {
        try {//Run.class.getResource("Images/LOGO.png")
            URL path = LanguageManager.class.getResource("lang/" + language + ".xml");
            languageFile = builder.build(path);
        } catch (java.io.FileNotFoundException ex) {
            JOptionPane.showMessageDialog(null, "Error reading language file", "DragMath", JOptionPane.ERROR_MESSAGE);
        } catch (IOException ex) {
            JOptionPane.showMessageDialog(null, "Error reading language file", "DragMath", JOptionPane.ERROR_MESSAGE);
        } catch (JDOMException ex) {
            JOptionPane.showMessageDialog(null, "Error reading language file", "DragMath", JOptionPane.ERROR_MESSAGE);
        }
        if (languageFile != null) {
            lang = languageFile.getRootElement();
        }
    }
}
