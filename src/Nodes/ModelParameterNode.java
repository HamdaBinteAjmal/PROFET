/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Nodes;

import Nodes.StatisticalDistributions.StatisticalDistribution;
import Nodes.StatisticalDistributions.TruncatedGaussian;

/**
 *
 * @author Administrator
 */
public class ModelParameterNode extends DBNNode  implements java.io.Serializable {
 
    private String Name;
    private boolean IsEvidenceNode = false;
    private StatisticalDistribution distribution = new TruncatedGaussian(0, 0, 0, 0);
    double std = 0;
    private boolean isOutputRequred = true;
    private double unscaledStd = 0;
    public ModelParameterNode(String name)
    {
        this.Name = name;
    }
    public ModelParameterNode(String name, StatisticalDistribution dist, 
            double std, double unscaledStd)
    {
        this.Name = name;
        this.distribution = dist;
        this.std = std;
        this.unscaledStd = unscaledStd;
    }
    @Override
    public boolean IsOutputRequired()
    {
        return isOutputRequred;
    }
    @Override
    public void SetOutputRequired(boolean required)
    {
        isOutputRequred = required;
    }
    public void SetUnscaledStd(double std)
    {
        this.unscaledStd = std;
    }
    public StatisticalDistribution GetDistribtion()
    {
        return this.distribution;
    }
    public void SetDistribtion(StatisticalDistribution dist)
    {
        this.distribution = dist;
    }
  
    public String GetName()
    {
        return this.Name;
    }
    public void SetStd(double timestep, double stepsize)
    {
        double scaledStd = this.unscaledStd / Math.sqrt(timestep/stepsize);
        //This is to avoid floating point overflow error in lisp 
      //  BigDecimal a = new BigDecimal(scaledStd);
       // BigDecimal roundOff = a.setScale(6, BigDecimal.ROUND_HALF_EVEN);

        //scaledStd =  roundOff.doubleValue();
       //this.unscaledStd = this.std;
        this.std = scaledStd;
    }
    public double GetStd()
    {
        return this.std;
    }
    public double GetUnscaledStd()
    {
        return this.unscaledStd;
    }
    
    @Override
    public String toString()
    {
       
        return this.Name;
        
    }
}
