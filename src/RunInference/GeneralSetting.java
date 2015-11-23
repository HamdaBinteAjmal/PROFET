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
package RunInference;

/**
 *
 * @author Administrator
 */
public class GeneralSetting  implements java.io.Serializable
{
    private double SummaryInterval = 1;
    private double Timespan = 10;
    private int Samples = 1000;
    private double StepSize = 1;
    private boolean fixedTimestep = true;
    
    public GeneralSetting()
    {
        
    }
    public GeneralSetting(GeneralSetting st)
    {
        SetTimespan(st.GetTimespan());
        SetSampleCount(st.GetSampleCount());
        SetStepSize(st.GetStepSize());
        SetSummaryIntervals(st.GetSummaryIntervals());
        SetFixedTimestep(st.isFixedTimestep());
    }
    public GeneralSetting(double tmspn, int sc, double ss, double sumInt)
    {
        SetTimespan(tmspn);
        SetSampleCount(sc);
        SetStepSize(ss);
        SetSummaryIntervals(sumInt);
    }
    public boolean isFixedTimestep()
    {
        return fixedTimestep;
    }
    
    public void SetFixedTimestep(boolean ft)
    {
        fixedTimestep = ft;
    }
     public double GetTimespan()
    {
        return Timespan;
    }
    public double GetSummaryIntervals()
    {
        return SummaryInterval;
        
    }
    public int GetSampleCount()
    {
        return Samples;
    }
    public  double GetStepSize()
    {
        return StepSize;
    }
    
    public void SetTimespan(double tm)
    {
        Timespan = tm;
        
    }
    public void SetSummaryIntervals(double SummInt)
    {
        SummaryInterval = SummInt;
    }
    public void SetSampleCount(int smCnt)
    {
        Samples = smCnt;
    }
    public void SetStepSize(double ss)
    {
        StepSize = ss;
    }
    
}
    
