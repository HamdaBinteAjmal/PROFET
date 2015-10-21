/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
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
    
