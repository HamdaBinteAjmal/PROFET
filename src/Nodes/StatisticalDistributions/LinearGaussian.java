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
public class LinearGaussian extends StatisticalDistribution {

    private double mean = 0;
    private double std = 0;
    public LinearGaussian(double mean, double std) {
        this.mean = mean;
        this.std = std;
    }

    public LinearGaussian(StatisticalDistribution dist) {
        if (dist.getClass() == Nodes.StatisticalDistributions.TruncatedGaussian.class)
        {
            TruncatedGaussian TG = (TruncatedGaussian)dist;
            this.mean = TG.GetMean();
            this.std = TG.GetStd();
        }
    }

    public void SetMean(double mean) {
        this.mean = mean;
    }

    public void SetStd(double std) {
        this.std = std;
    }

    
    public double GetMean() {
        return this.mean;
    }

    
    public double GetStd() {
        return this.std;
    }

}
