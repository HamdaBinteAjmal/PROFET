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
