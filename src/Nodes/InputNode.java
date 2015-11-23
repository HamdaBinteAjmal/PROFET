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
package Nodes;

import RunInference.Evidence;

/**
 *
 * @author Administrator
 */

public class InputNode extends DBNNode  implements java.io.Serializable {
    private double mean = 0;
    private double std = 0;
    private String name;
    private boolean isObserveableNode = false;
    private double parentStd = 0;
    private double parentCoefficient = 1;
    private double parentOffset = 0;
    private boolean isOutputRequred = true;
    ModelVariableNode evidenceParent = null;
    
    Evidence evidence = new Evidence();
    public InputNode(String name)
    {
        this.name = /*"Intended-" + */name;
        evidence  = new Evidence();
    }
    
    public InputNode (boolean isEvidenceNode, ModelVariableNode parent)
    {
        if(!isEvidenceNode)
        {
            //this.name = /*"Intended-" +*/ name;
        }
        else
        {
            this.name = "Observed-" + parent.GetName();
            this.isObserveableNode = true;
            this.evidenceParent = parent;
        }
        evidence = new Evidence();
        //this.mean = mean;
        //this.std = std;     
    }
    public String GetEvidenceName()
    {
        String name = this.toString() + "Evidence";
        return name;
    }
    public boolean IsOutputRequired()
    {
        return isOutputRequred;
    }
    public void SetOutputRequired(boolean required)
    {
        isOutputRequred = required;
    }
    public Evidence GetEvidence()
    {
        return this.evidence;
    }
    public ModelVariableNode GetEvidenceParent()
    {
        return this.evidenceParent;
    }
    public void SetMean(double mean)
    {
        this.mean = mean;
    }
    public void SetStd(double std)
    {
        this.std = std;
    }
    public double GetMean()
    {
        return this.mean;
    }
    public double GetStd()
    {
        return this.std;
    }
    
    public void SetParentStd(double std)
    {
        this.parentStd = std;
    }
    public void SetParentCoefficient(double coff)
    {
        this.parentCoefficient = coff;
    }
    public void SetParentOffset(double offset)
    {
        this.parentOffset = offset;
    }
    
    public double GetParentStd()
    {
        return this.parentStd;
    }
    public double GetParentCoefficient()
    {
        return this.parentCoefficient;
    }
   
    public double GetParentOffset()
    {
        return this.parentOffset;
    }
    public boolean IsObserveableNode()
    {
        return this.isObserveableNode;
    }
    @Override
    public String toString()
    {
        return this.name;
    }

    public void SetEvidence(Evidence evidence) {
        this.evidence = evidence;
        }
    
}