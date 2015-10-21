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
public class Tolerance implements java.io.Serializable{
    private double tolerance = 0.01;

    public Tolerance() {
        tolerance = 0.01;
    }
    
    
    public double GetTolerance()
    {
        return tolerance;
    }
    public void SetTolerance(double t)
    {
        tolerance = t;
    }
}