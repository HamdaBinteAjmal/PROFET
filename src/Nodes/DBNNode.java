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
public abstract class DBNNode {
    @Override
    public abstract String toString();
    public abstract void SetOutputRequired(boolean b);
    public abstract boolean IsOutputRequired();
}
