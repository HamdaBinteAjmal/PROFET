/* 
 * PROFET Copyright 2015 (c) Data Mining and Machine Learning Group,
 * National University of Ireland Galway.  
 * This file is a part of PROFET  
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
package MainInterface;

import java.io.File;
import javax.swing.filechooser.FileFilter;

/**
 *
 * @author Administrator
 */
public class ProjectFileFilter extends FileFilter{
    // Rename to project name, when decided

    
    
    /** Creates a new instance of ProjectFileFilter */
    public ProjectFileFilter() {
    }
    
    
    public boolean accept(File f) {
        if(f != null) {
            if (f.isDirectory()) return true;
            String filename = f.getName();
            int i = filename.lastIndexOf('.');
            if(i > 0 && i < filename.length() - 1) {
                if (filename.substring(i+1).toLowerCase().equals("prft")) return true;
            }
        }
        return false;
    }
    
    public static boolean isPrftFile(File f) {
        if(f != null) {
            String filename = f.getName();
            int i = filename.lastIndexOf('.');
            if(i > 0 && i < filename.length() - 1) {
                if (filename.substring(i+1).toLowerCase().equals("prft")) return true;
                if (f.isDirectory()) return true;
            }
        }
        return false;
    }
    
    public String getDescription(){
        return "Profet Project (*.prft)";
    }


    
}
