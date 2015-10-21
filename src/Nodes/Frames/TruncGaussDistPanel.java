/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Nodes.Frames;

import Nodes.StatisticalDistributions.TruncatedGaussian;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 *
 * @author Administrator
 */
public class TruncGaussDistPanel extends javax.swing.JPanel {

    /**
     * Creates new form TruncGaussDistPanel
     */
    private TruncatedGaussian distribution;
    public TruncGaussDistPanel(TruncatedGaussian dist) {
        initComponents();
        this.distribution = dist;
        jTextFieldLowerLimit.setText(Double.toString(distribution.GetLowerLimit()));
        jTextFieldUpperLimit.setText(Double.toString(distribution.GetUpperLimit()));
        jTextFieldMean.setText(Double.toString(distribution.GetMean()));
        jTextFieldStd.setText(Double.toString(distribution.GetStd()));
        AddChangeListeners();
    }
    private void AddChangeListeners()
     {
         AddChangeListener(jTextFieldUpperLimit);
         AddChangeListener(jTextFieldLowerLimit);
         AddChangeListener(jTextFieldMean);
         AddChangeListener(jTextFieldStd);
     }
     private void AddChangeListener(JTextField field)
    {
         //ModelParameterNodeWindow f = (ModelParameterNodeWindow)javax.swing.SwingUtilities.getWindowAncestor(this);
//        //VERY VERY VERY BAD PROGRAMMING PRACTICE 
 field.getDocument().addDocumentListener(new DocumentListener() {
        @Override
        public void changedUpdate(DocumentEvent e) {
           ModelParameterNodeWindow p = (ModelParameterNodeWindow) getParent().getParent();
           p.UpdateButtonChangeState(true);
           // parent.UpdateButtonChangeState(true);
            // text was changed
        }
        @Override
        public void removeUpdate(DocumentEvent e) {
            
            ModelParameterNodeWindow p = (ModelParameterNodeWindow) getParent().getParent();
           p.UpdateButtonChangeState(true);
            
           // parent.UpdateButtonChangeState(true);
            // text was deleted
        }
       @Override
        public void insertUpdate(DocumentEvent e) {
          ModelParameterNodeWindow p = (ModelParameterNodeWindow) getParent().getParent();
           p.UpdateButtonChangeState(true);
            ///parent.UpdateButtonChangeState(true);
            // text was inserted
       }
        
    });
         }
     private boolean isNumeric(String str)
    {
        try 
        {  
            double d = Double.parseDouble(str);  
        }  
        catch(NumberFormatException nfe)  
        {  
            return false;  
        }  
        return true;  
    }
     private void SetDistribution()
     {
         distribution.SetLowerLimit(Double.parseDouble(jTextFieldLowerLimit.getText()));
         distribution.SetUpperLimit(Double.parseDouble(jTextFieldUpperLimit.getText()));
         distribution.SetMean(Double.parseDouble(jTextFieldMean.getText()));
         distribution.SetStd(Double.parseDouble(jTextFieldStd.getText()));
     }
     
     public boolean Validate()
     {
         
         boolean retVal = false;
         
         if (!isEverythingNumeric())
         {
             
              JOptionPane.showMessageDialog(this, "Incorrect Input:\n "
                    + "Please Enter numeric values only", 
                   "Error", JOptionPane.ERROR_MESSAGE);
                retVal = false;
         }
         else
         {
             double lowerLimit = Double.parseDouble(jTextFieldLowerLimit.getText());
             double upperLimit = Double.parseDouble(jTextFieldUpperLimit.getText());
             double mean = Double.parseDouble(jTextFieldMean.getText());
             
             if (upperLimit < lowerLimit)
             {
                   JOptionPane.showMessageDialog(this, "Incorrect Input:\n "
                    + "Upper Limit can not be less than the lower limit", 
                   "Error", JOptionPane.ERROR_MESSAGE);
                retVal = false;
                 
             }
             else if(mean > upperLimit)
             {
                 JOptionPane.showMessageDialog(this, "Incorrect Input:\n "
                    + "Mean can not be greater than the upper limit", 
                   "Error", JOptionPane.ERROR_MESSAGE);
                retVal = false;
                 
             }
             else if (mean < lowerLimit)
             {
                 JOptionPane.showMessageDialog(this, "Incorrect Input:\n "
                    + "Mean can not be less than the lower limit", 
                   "Error", JOptionPane.ERROR_MESSAGE);
                retVal = false;
             }
             else
             {
                 SetDistribution();
                 retVal = true;
             }
         }
         return retVal;
     }
     private boolean isEverythingNumeric()
     {
        if (isNumeric(jTextFieldLowerLimit.getText())
                && isNumeric(jTextFieldUpperLimit.getText())
                && isNumeric(jTextFieldMean.getText())
                && isNumeric(jTextFieldStd.getText()))
                {
                    return true;
                }
                
        else
        {
                return false;
        }
     }
    public TruncatedGaussian GetDistribution()
    {
        return distribution;
        
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jTextFieldUpperLimit = new javax.swing.JTextField();
        jTextFieldLowerLimit = new javax.swing.JTextField();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jTextFieldMean = new javax.swing.JTextField();
        jTextFieldStd = new javax.swing.JTextField();

        setPreferredSize(new java.awt.Dimension(547, 100));

        jTextFieldUpperLimit.setText("jTextField1");

        jTextFieldLowerLimit.setText("jTextField2");

        jLabel1.setText("Upper Limit:");

        jLabel2.setText("Lower Limit:");

        jLabel3.setLabelFor(jTextFieldMean);
        jLabel3.setText("Mean:");

        jLabel4.setLabelFor(jTextFieldStd);
        jLabel4.setText("Std. Dev:");

        jTextFieldMean.setText("jTextField1");

        jTextFieldStd.setText("jTextField2");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(112, 112, 112)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jTextFieldUpperLimit, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel2)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jTextFieldLowerLimit, javax.swing.GroupLayout.PREFERRED_SIZE, 100, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addGap(36, 36, 36)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jLabel3)
                    .addComponent(jLabel4))
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(jTextFieldMean, javax.swing.GroupLayout.DEFAULT_SIZE, 100, Short.MAX_VALUE)
                    .addComponent(jTextFieldStd))
                .addContainerGap(68, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(16, 16, 16)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldUpperLimit, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel3)
                    .addComponent(jTextFieldMean, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(30, 30, 30)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jTextFieldLowerLimit, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel4)
                    .addComponent(jTextFieldStd, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addContainerGap(30, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JTextField jTextFieldLowerLimit;
    private javax.swing.JTextField jTextFieldMean;
    private javax.swing.JTextField jTextFieldStd;
    private javax.swing.JTextField jTextFieldUpperLimit;
    // End of variables declaration//GEN-END:variables
}
