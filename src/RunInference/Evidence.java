/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package RunInference;

import java.io.File;

/**
 *
 * @author Administrator
 */
public class Evidence implements java.io.Serializable{
    //private InputNode node;
    private boolean continous;
    private File csvFile;
    public Evidence()
    {
        //this.node = node;
        csvFile = null;
        this.continous  = true;
    }
    public boolean isContinous()
    {
        return continous;
    }
    public File GetFile()
    {
        if (csvFile == null)
        {
            return null;
        }
        else
        {
            return csvFile;
        }
    }
    public void SetFile(File file)
    {
        this.csvFile = file;
    }
    public void SetContinous()
    {
        this.continous = true;
    }
    public void SetInstantaneous()
    {
        this.continous = false;
    }
//    @Override
//    public String toString()
//    {
//        return this.node.toString();
//    }
    
}
