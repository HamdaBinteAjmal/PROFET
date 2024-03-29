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

import MainInterface.Timestep;
import MainInterface.Tree;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 *
 * @author Administrator
 */
public class GeneralSettingsWindow {
    public static class GeneralSettingsInner extends javax.swing.JPanel implements java.io.Serializable
    {

        public static GeneralSetting setting;
    /**
     * Creates new form GeneralSettingsWindow
     */
   // private ArrayList<InputNode> inputNodes = new ArrayList<>();
   
        
        //private double DefinedTimstep = 1;
   
    
    public GeneralSettingsInner() {
        initComponents();
        setting = new GeneralSetting();
        AddChangeListener(jTextFieldNumberOfSamples);
        AddChangeListener(jTextFieldSummaryInterval);
        AddChangeListener(jTextFieldTimespan);
        AddChangeListener(jTextFieldStepSize);
        
        jTextFieldNumberOfSamples.setText(String.valueOf(setting.GetSampleCount()));
        jTextFieldSummaryInterval.setText(String.valueOf(setting.GetSummaryIntervals()));
        jTextFieldTimespan.setText(String.valueOf(setting.GetTimespan()));
        jTextFieldStepSize.setText(String.valueOf(Timestep.TimestepInner.GetTimestep()));
        jButtonUpdate.setEnabled(false);
        
        //Need to refresh
        jLabelDefinedTimestep.setText("Defined Timestep is :" + String.valueOf(Timestep.TimestepInner.GetTimestep()));
        
        FixedTimestep.setSelected(true);
        jLabelSummary.setEnabled(!FixedTimestep.isSelected());
        
        jTextFieldSummaryInterval.setEnabled(!FixedTimestep.isSelected());
        
        jLabelDefinedTimestep.setEnabled(FixedTimestep.isSelected());
        jTextFieldStepSize.setEnabled(FixedTimestep.isSelected());
        setting.SetStepSize(Timestep.TimestepInner.GetTimestep());
    }
    
    public void SetSetting(GeneralSetting stng) {
        
        setting = new GeneralSetting(stng);
        
        
        jTextFieldNumberOfSamples.setText(String.valueOf(setting.GetSampleCount()));
        jTextFieldSummaryInterval.setText(String.valueOf(setting.GetSummaryIntervals()));
        jTextFieldTimespan.setText(String.valueOf(setting.GetTimespan()));
      
        jTextFieldStepSize.setText(String.valueOf(setting.GetStepSize()));
        jButtonUpdate.setEnabled(false);
        
        //Need to refresh
        jLabelDefinedTimestep.setText("Defined Timestep is :" + String.valueOf(Timestep.TimestepInner.GetTimestep()));
        
        FixedTimestep.setSelected(setting.isFixedTimestep());
        AdaptiveTimestep.setSelected(!setting.isFixedTimestep());
        jLabelSummary.setEnabled(!FixedTimestep.isSelected());
        
        jTextFieldSummaryInterval.setEnabled(!FixedTimestep.isSelected());
        
        jLabelDefinedTimestep.setEnabled(FixedTimestep.isSelected());
        jTextFieldStepSize.setEnabled(FixedTimestep.isSelected());
        this.setVisible(true);
        
       // StepSize = Timestep.TimestepInner.GetTimestep();
    } 
    
    public GeneralSetting GetSetting()
    {
        return setting;
    }
    public GeneralSettingsInner(GeneralSetting st) {
        initComponents();
        AddChangeListener(jTextFieldNumberOfSamples);
        AddChangeListener(jTextFieldSummaryInterval);
        AddChangeListener(jTextFieldTimespan);
        AddChangeListener(jTextFieldStepSize);
        setting = new GeneralSetting(st);
        
        jTextFieldNumberOfSamples.setText(String.valueOf(setting.GetSampleCount()));
        jTextFieldSummaryInterval.setText(String.valueOf(setting.GetSummaryIntervals()));
        jTextFieldTimespan.setText(String.valueOf(setting.GetTimespan()));
        jTextFieldStepSize.setText(String.valueOf(Timestep.TimestepInner.GetTimestep()));
        jButtonUpdate.setEnabled(false);
        
        //Need to refresh
        jLabelDefinedTimestep.setText("Defined Timestep is :" + String.valueOf(Timestep.TimestepInner.GetTimestep()));
        
        FixedTimestep.setSelected(st.isFixedTimestep());
        AdaptiveTimestep.setSelected(!st.isFixedTimestep());
        jLabelSummary.setEnabled(!FixedTimestep.isSelected());
        
        jTextFieldSummaryInterval.setEnabled(!FixedTimestep.isSelected());
        
        jLabelDefinedTimestep.setEnabled(FixedTimestep.isSelected());
        jTextFieldStepSize.setEnabled(FixedTimestep.isSelected());
        setting.SetStepSize( Timestep.TimestepInner.GetTimestep());
    }
       /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonGroupSetting = new javax.swing.ButtonGroup();
        FixedTimestep = new javax.swing.JRadioButton();
        AdaptiveTimestep = new javax.swing.JRadioButton();
        jLabelDefinedTimestep = new javax.swing.JLabel();
        jLabelSummary = new javax.swing.JLabel();
        jLabelSamples = new javax.swing.JLabel();
        jLabelTimespan = new javax.swing.JLabel();
        jTextFieldSummaryInterval = new javax.swing.JTextField();
        jTextFieldNumberOfSamples = new javax.swing.JTextField();
        jTextFieldTimespan = new javax.swing.JTextField();
        jButtonUpdate = new javax.swing.JButton();
        jLabelStepSize = new javax.swing.JLabel();
        jTextFieldStepSize = new javax.swing.JTextField();

        setMinimumSize(new java.awt.Dimension(800, 560));
        setPreferredSize(new java.awt.Dimension(800, 560));

        buttonGroupSetting.add(FixedTimestep);
        FixedTimestep.setText("Perform Standard Fixed Time Step Particle Filtering");
        FixedTimestep.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                FixedTimestepActionPerformed(evt);
            }
        });

        buttonGroupSetting.add(AdaptiveTimestep);
        AdaptiveTimestep.setText("Perform Adaptive-Time Particle Filtering");
        AdaptiveTimestep.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                AdaptiveTimestepActionPerformed(evt);
            }
        });

        jLabelDefinedTimestep.setText("Natural Timestep of DBN is : ?");

        jLabelSummary.setText("Summary Interval:");

        jLabelSamples.setText("No. of Samples:");

        jLabelTimespan.setText("Timespan (Total time to Run inference):");

        jTextFieldSummaryInterval.setText("jTextField1");

        jTextFieldNumberOfSamples.setText("jTextField2");

        jTextFieldTimespan.setText("jTextField3");

        jButtonUpdate.setText("Set");
        jButtonUpdate.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonUpdateActionPerformed(evt);
            }
        });

        jLabelStepSize.setText("Step Size:");

        jTextFieldStepSize.setText("jTextField1");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(258, 258, 258)
                        .addComponent(jButtonUpdate, javax.swing.GroupLayout.PREFERRED_SIZE, 77, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(layout.createSequentialGroup()
                        .addGap(58, 58, 58)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabelDefinedTimestep)
                            .addComponent(AdaptiveTimestep)
                            .addComponent(FixedTimestep)
                            .addGroup(layout.createSequentialGroup()
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jLabelSamples)
                                    .addComponent(jLabelTimespan)
                                    .addComponent(jLabelStepSize)
                                    .addComponent(jLabelSummary))
                                .addGap(55, 55, 55)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jTextFieldSummaryInterval, javax.swing.GroupLayout.PREFERRED_SIZE, 98, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                                        .addComponent(jTextFieldTimespan, javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jTextFieldNumberOfSamples, javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jTextFieldStepSize, javax.swing.GroupLayout.PREFERRED_SIZE, 98, javax.swing.GroupLayout.PREFERRED_SIZE)))))))
                .addContainerGap(399, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(52, 52, 52)
                .addComponent(FixedTimestep)
                .addGap(18, 18, 18)
                .addComponent(AdaptiveTimestep)
                .addGap(77, 77, 77)
                .addComponent(jLabelDefinedTimestep)
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabelSummary)
                    .addComponent(jTextFieldSummaryInterval, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextFieldNumberOfSamples, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabelSamples))
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabelTimespan)
                    .addComponent(jTextFieldTimespan, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jTextFieldStepSize, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabelStepSize))
                .addGap(49, 49, 49)
                .addComponent(jButtonUpdate)
                .addContainerGap(129, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    private void FixedTimestepActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_FixedTimestepActionPerformed
        // TODO add your handling code here:
        jTextFieldSummaryInterval.setEnabled(!FixedTimestep.isSelected());
        jLabelSummary.setEnabled(!FixedTimestep.isSelected());
        jLabelStepSize.setEnabled(FixedTimestep.isSelected());
        jTextFieldStepSize.setEnabled(FixedTimestep.isSelected());
        //setting.SetFixedTimestep(true);
        jButtonUpdate.setEnabled(true);
    }//GEN-LAST:event_FixedTimestepActionPerformed

    private void jButtonUpdateActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonUpdateActionPerformed
        // TODO add your handling code here:
        
        if (Validate())
        {
            setting.SetSummaryIntervals(Double.valueOf(jTextFieldSummaryInterval.getText()));
            setting.SetTimespan(Double.valueOf(jTextFieldTimespan.getText()));
            setting.SetSampleCount(Integer.valueOf(jTextFieldNumberOfSamples.getText()));
            setting.SetStepSize(Double.valueOf(jTextFieldStepSize.getText()));
            setting.SetFixedTimestep(FixedTimestep.isSelected());
            jButtonUpdate.setEnabled(false);
            Tree.TreeStatic.ResetSTDforModelParameters();
                
            
        }
        
    }//GEN-LAST:event_jButtonUpdateActionPerformed

    private void AdaptiveTimestepActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_AdaptiveTimestepActionPerformed
        // TODO add your handling code here:
       
            setting.SetStepSize(Timestep.TimestepInner.GetTimestep()); //in case of adaptive set it equal to natural timestep
            jTextFieldStepSize.setText(String.valueOf(Timestep.TimestepInner.GetTimestep()));
            jTextFieldSummaryInterval.setEnabled(AdaptiveTimestep.isSelected());
            jLabelSummary.setEnabled(AdaptiveTimestep.isSelected());
            jLabelStepSize.setEnabled(!AdaptiveTimestep.isSelected());
            jTextFieldStepSize.setEnabled(!AdaptiveTimestep.isSelected());
            //setting.SetFixedTimestep(false);
            jButtonUpdate.setEnabled(true);
    }//GEN-LAST:event_AdaptiveTimestepActionPerformed
   
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
    public void SetNaturalTimestepLabel(double naturalTimestep)
    {
        jLabelDefinedTimestep.setText("Natural Timestep is " + String.valueOf(naturalTimestep));
        if (AdaptiveTimestep.isSelected())
        {
            setting.SetStepSize(naturalTimestep);
            jTextFieldStepSize.setText(String.valueOf(setting.GetStepSize()));
        }
        repaint();
        
    }
    boolean Validate()
    {
         boolean retVal = true;
        
        if (!isNumeric(jTextFieldNumberOfSamples.getText())
                || !isNumeric(jTextFieldTimespan.getText()))
        {
            retVal = false;
           
        }
        if (FixedTimestep.isSelected() 
                && !isNumeric(jTextFieldStepSize.getText()))
        {
            retVal = false;
        }
        if (AdaptiveTimestep.isSelected() 
                && !isNumeric(jTextFieldSummaryInterval.getText()))
        {
            retVal = false;
        }
        
        if (retVal == false)
        {
             JOptionPane.showMessageDialog(this, "Incorrect Input:\n "
                    + "Please Enter numeric values only", 
                    "Error", JOptionPane.ERROR_MESSAGE);
        }
        else
        {
            double SumInt = Double.valueOf(jTextFieldSummaryInterval.getText());
            double Tmspn = Double.valueOf(jTextFieldTimespan.getText());
            double smple = Double.valueOf(jTextFieldNumberOfSamples.getText());
            double stpsize = Double.valueOf(jTextFieldStepSize.getText());
            
            String errorMessage = "";
            if (SumInt <= 0 && AdaptiveTimestep.isSelected())
            {
                errorMessage = errorMessage + "\nSummary Interval can not be zero or less";
                retVal = false;
            }
            if (stpsize <=0 && FixedTimestep.isSelected())
            {
                errorMessage = errorMessage + "\nStep size can not be zero or less";
                retVal = false;
            }
            if (Tmspn <= 0)
            {
                errorMessage = errorMessage + "\nTimespan can not be zero or less";
                retVal = false;
            }
            if (smple <= 0)
            {
                errorMessage = errorMessage + "\nNo. of samples can not be zero or less";
                retVal = false;
            }
            if (!errorMessage.equals(""))
            {
                JOptionPane.showMessageDialog(this, errorMessage, 
                    "Error", JOptionPane.ERROR_MESSAGE);
                retVal = false;
            }
        }
        return retVal;
        
    }
     private void AddChangeListener(JTextField field)
    {
        field.getDocument().addDocumentListener(new DocumentListener() {
        @Override
        public void changedUpdate(DocumentEvent e) {
            jButtonUpdate.setEnabled(true);
            // text was changed
        }
        @Override
        public void removeUpdate(DocumentEvent e) {
            jButtonUpdate.setEnabled(true);
            // text was deleted
        }
        @Override
        public void insertUpdate(DocumentEvent e) {
            jButtonUpdate.setEnabled(true);
            // text was inserted
        }
        });
        
    }
     public boolean isFixedTimestep()
     {
         return FixedTimestep.isSelected();
     }
     @Override
    public String toString()
    {
        return "General Settings";
    }
    
   

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JRadioButton AdaptiveTimestep;
    private javax.swing.JRadioButton FixedTimestep;
    private javax.swing.ButtonGroup buttonGroupSetting;
    private javax.swing.JButton jButtonUpdate;
    private javax.swing.JLabel jLabelDefinedTimestep;
    private javax.swing.JLabel jLabelSamples;
    private javax.swing.JLabel jLabelStepSize;
    private javax.swing.JLabel jLabelSummary;
    private javax.swing.JLabel jLabelTimespan;
    private javax.swing.JTextField jTextFieldNumberOfSamples;
    private javax.swing.JTextField jTextFieldStepSize;
    private javax.swing.JTextField jTextFieldSummaryInterval;
    private javax.swing.JTextField jTextFieldTimespan;
    // End of variables declaration//GEN-END:variables
    }
}