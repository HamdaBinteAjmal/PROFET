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
