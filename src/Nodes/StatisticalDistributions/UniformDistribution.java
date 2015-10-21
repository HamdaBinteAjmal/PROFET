/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Nodes.StatisticalDistributions;


/**
 *
 * @author Administrator
 */
public class UniformDistribution extends StatisticalDistribution {
    private double UpperLimit = 0;
    private double LowerLimit = 0;
    public UniformDistribution(double upperLimit, double lowerLimit)
    {
        this.UpperLimit = upperLimit;
        this.LowerLimit = lowerLimit;
    }
    public UniformDistribution(StatisticalDistribution dist)
    {
        if (dist.getClass() == Nodes.StatisticalDistributions.TruncatedGaussian.class)
        {
            TruncatedGaussian TG = (TruncatedGaussian)dist;
            this.UpperLimit = TG.GetUpperLimit();
            this.LowerLimit = TG.GetLowerLimit();
        }
//        else if(dist.getClass() == Nodes.StatisticalDistributions.UniformDistribution.class)
//        {
//            UniformDistribution UD = (UniformDistribution) dist;
//            this.UpperLimit = UD.GetUpperLimit();
//            this.LowerLimit = UD.GetLowerLimit();
//        }
    }
    public void SetUpperLimit(double upperLimit)
    {
        this.UpperLimit = upperLimit;
    }
    public void SetLowerLimit(double lowerLimit)
    {
        this.LowerLimit = lowerLimit;
    }
   
    public double GetUpperLimit()
    {
        return this.UpperLimit;
    }
    
    public double GetLowerLimit()
    {
        return this.LowerLimit;
    }
}
