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
