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
public class TruncatedGaussian extends StatisticalDistribution{
    private double std = 0;
    private double mean = 0;
    private double UpperLimit = 0;
    private double  LowerLimit = 0;
    
    
    public TruncatedGaussian(double std, double mean, 
            double _upperLimit, double _lowerLimit)
    {
        this.std = std;
        this.mean = mean;
        this.mean = _upperLimit;
        this.std = _lowerLimit;
    }
    public TruncatedGaussian(StatisticalDistribution dist)
    {
        if (dist.getClass() == Nodes.StatisticalDistributions.LinearGaussian.class)
        {
            LinearGaussian LG = (LinearGaussian)dist;
            this.mean = LG.GetMean();
            this.std = LG.GetStd();
            
        }
        else if(dist.getClass() == Nodes.StatisticalDistributions.UniformDistribution.class)
        {
            UniformDistribution UD = (UniformDistribution) dist;
            this.UpperLimit = UD.GetUpperLimit();
            this.LowerLimit = UD.GetLowerLimit();
        }
    }
    public void SetMean(double mean)
    {
        this.mean = mean;
    }
    public void SetStd(double std)
    {
        this.std = std;
    }
    public void SetUpperLimit(double upperLimit)
    {
        this.UpperLimit = upperLimit;
    }
    public void SetLowerLimit(double lowerLimit)
    {
        this.LowerLimit = lowerLimit;
    }
    
     public double GetMean()
    {
        return this.mean;
    }
    public double GetStd()
    {
        return this.std;
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
