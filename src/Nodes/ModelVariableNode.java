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

import Nodes.StatisticalDistributions.StatisticalDistribution;

/**
 *
 * @author Administrator
 */
public class ModelVariableNode extends DBNNode  implements java.io.Serializable {
    private double initialValue = 0;
    public boolean alsoAnEvidenceNode = false;
    private String Name;
    private boolean isOutputRequred = true;
    private boolean imposeTolerance = true;
    //private StatisticalDistribution distribution = new TruncatedGaussian(0, 0, 0, 0);
    
    public ModelVariableNode(String name)
    {
        this.Name = name;
    }
    public ModelVariableNode(String name, double initialValue, 
            StatisticalDistribution dist)
    {
        this.Name = name;
        this.initialValue = initialValue;
       
    }
    public boolean IsOutputRequired()
    {
        return isOutputRequred;
    }
    public void SetOutputRequired(boolean required)
    {
        isOutputRequred = required;
    }
    public String ToString()
    {
        return this.Name;
    }
    public void SetInitialValue(double val)
    {
        initialValue = val;
    }
    public double GetInitialValue()
    {
        return initialValue;
    }
    public String GetName()
    {
        return this.Name;
    
    }
    public InputNode AddAnEvidenceNode(boolean add)
    {
        this.alsoAnEvidenceNode = add;
        InputNode evidenceNode = null;
        if(add)
        {
             evidenceNode = new InputNode(alsoAnEvidenceNode, this);
             GlobalLists.AddEvidenceNode(evidenceNode);
             //TODO: Add the node here
        } 
        else
        {
            //evidenceNode = new InputNode(true, this); //temp
           GlobalLists.RemoveEvidenceNode(this);// Handle Delete
        }
        return evidenceNode;
    }
    public boolean AlsoAnEvidenceNode()
    {
        return this.alsoAnEvidenceNode;
    }
    @Override
    public String toString()
    {
        return this.Name;
    }
    public boolean ImposeTolerance()
    {
        return imposeTolerance;
    }
    public void ImposeTolerance(boolean impose)
    {
        imposeTolerance = impose;
    }
}
