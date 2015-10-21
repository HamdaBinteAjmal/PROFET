/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
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
