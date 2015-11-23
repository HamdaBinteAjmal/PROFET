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
package MainInterface;

import javax.swing.JOptionPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 *
 * @author Administrator
 */

    public class Timestep
    {
        public static class TimestepInner  extends javax.swing.JPanel {
        static double timestepValue = 1;
        public TimestepInner() 
        {
            initComponents();
            timestepValue = 1;
            TimestepTextBox.setText("1");
            AddDocumentListenter();
            //this.setBackground(Color.WHITE);
        }
         public TimestepInner(double timestep) 
        {
            initComponents();
            timestepValue = timestep;
            TimestepTextBox.setText(String.valueOf(timestep));
            AddDocumentListenter();
            //this.setBackgrotund(Color.WHITE);
        }
        private boolean CheckError()
        {
            boolean retVal = true;
            try  
            {  
                double d = Double.parseDouble(TimestepTextBox.getText());  
                if ( d == 0)
                {
                    throw new NumberFormatException();
                }
                timestepValue = d;
                jButtonUpdate.setEnabled(false);
            }  
            catch(NumberFormatException nfe)  
            {  
                retVal = false;
                //TimestepTextBox.setText("0");
                JOptionPane.showMessageDialog(null, "Invalid Timestep", "Error Message",
                        JOptionPane.ERROR_MESSAGE);
            }
            return retVal;
    }
    @Override
    public String toString()
    {
        return "Time step";
    }
    public void SetTimestep(double timestep)
    {
        timestepValue = timestep;
        TimestepTextBox.setText(String.valueOf(timestep));
    }
    public static double GetTimestep()
    {
        return timestepValue;
    }
    
    private void AddDocumentListenter()
    {
        TimestepTextBox.getDocument().addDocumentListener(new DocumentListener() 
        {
        @Override
        public void changedUpdate(DocumentEvent e) 
        {
            jButtonUpdate.setEnabled(true);// = true;
        }
        @Override
        public void removeUpdate(DocumentEvent e) {
            jButtonUpdate.setEnabled(true);
        }
        @Override
        public void insertUpdate(DocumentEvent e) {
            jButtonUpdate.setEnabled(true);
        }

       
  }
);
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel1 = new javax.swing.JLabel();
        TimestepTextBox = new javax.swing.JTextField();
        jButtonUpdate = new javax.swing.JButton();

        setBorder(javax.swing.BorderFactory.createEtchedBorder());
        setMinimumSize(new java.awt.Dimension(800, 560));
        setPreferredSize(new java.awt.Dimension(800, 560));

        jLabel1.setText("Enter the natural time step for the DBN : ");

        TimestepTextBox.setText("1");
        TimestepTextBox.setToolTipText("");
        TimestepTextBox.setName("TimestepTextBox"); // NOI18N

        jButtonUpdate.setText("Update");
        jButtonUpdate.setEnabled(false);
        jButtonUpdate.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonUpdateActionPerformed(evt);
            }
        });

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(76, 76, 76)
                        .addComponent(jLabel1)
                        .addGap(18, 18, 18)
                        .addComponent(TimestepTextBox, javax.swing.GroupLayout.PREFERRED_SIZE, 119, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(layout.createSequentialGroup()
                        .addGap(215, 215, 215)
                        .addComponent(jButtonUpdate)))
                .addContainerGap(385, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(56, 56, 56)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(TimestepTextBox, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addComponent(jButtonUpdate)
                .addContainerGap(439, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    private void jButtonUpdateActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonUpdateActionPerformed
        // TODO add your handling code here:
        if (CheckError())
        {
            SetTimestep(Double.valueOf(TimestepTextBox.getText()));
            Tree.TreeStatic.ResetSTDforModelParameters();
            jButtonUpdate.setEnabled(false);
        }
       
    }//GEN-LAST:event_jButtonUpdateActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JTextField TimestepTextBox;
    private javax.swing.JButton jButtonUpdate;
    private javax.swing.JLabel jLabel1;
    // End of variables declaration//GEN-END:variables
}
    }