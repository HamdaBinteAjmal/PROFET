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
