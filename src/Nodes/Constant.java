/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Nodes;

/**
 *
 * @author Administrator
 */
public class Constant  implements java.io.Serializable {
    private String name;
    private double value = 0.0;
    public Constant(String name)
    {
        this.name = name;
    }
    public Constant(String name, double value)
    {
        this.name = name;
        this.value = value;
    }
    @Override
    public String toString()
    {
        return this.name;
    }
    public void SetValue(double val)
    {
        this.value = val;
    }
    public double GetValue()
    {
        return this.value;
    }
}
