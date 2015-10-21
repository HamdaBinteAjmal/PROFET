/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
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
