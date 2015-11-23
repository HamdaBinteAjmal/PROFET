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
package Nodes.Frames;

import Nodes.InputNode;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

/**
 *
 * @author Administrator
 */
public class ObservedPanel extends javax.swing.JPanel {

    /**
     * Creates new form ObservedPanel
     */
    InputNode node ;
    public ObservedPanel(InputNode n) {
        initComponents();
        node = n;
        
        FillForm();
        AddChangeListeners();
    }
    private void AddChangeListeners()
     {
         AddChangeListener(jTextFieldParentCoefficient);
         AddChangeListener(jTextFieldParentOffset);
         AddChangeListener(jTextFieldParentStd);
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
    private void FillForm()
    {
          
        jTextFieldParentStd.setText(String.valueOf(node.GetParentStd()));// = node.GetParentStd();
        jTextFieldParentCoefficient.setText(String.valueOf(node.GetParentCoefficient()));// = node.GetParentCoefficient();
        jTextFieldParentOffset.setText(String.valueOf(node.GetParentOffset()));// = ;
        
        jLabelName.setText(node.toString());
        this.jButtonUpdate.setEnabled(false);
      
        StyledDocument doc = jTextPane1.getStyledDocument();
        SimpleAttributeSet center = new SimpleAttributeSet();
        StyleConstants.setAlignment(center, StyleConstants.ALIGN_JUSTIFIED);
        doc.setParagraphAttributes(0, doc.getLength(), center, false);
               
    }
    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jScrollPane2 = new javax.swing.JScrollPane();
        jTextPane1 = new javax.swing.JTextPane();
        jLabel1 = new javax.swing.JLabel();
        jLabelName = new javax.swing.JLabel();
        jTextFieldParentStd = new javax.swing.JTextField();
        jLabel8 = new javax.swing.JLabel();
        jLabel9 = new javax.swing.JLabel();
        jTextFieldParentCoefficient = new javax.swing.JTextField();
        jTextFieldParentOffset = new javax.swing.JTextField();
        jLabel10 = new javax.swing.JLabel();
        jButtonUpdate = new javax.swing.JButton();
        jLabel3 = new javax.swing.JLabel();

        setBorder(javax.swing.BorderFactory.createEtchedBorder());
        setMinimumSize(new java.awt.Dimension(600, 560));
        setPreferredSize(new java.awt.Dimension(600, 560));

        jScrollPane2.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

        jTextPane1.setEditable(false);
        jTextPane1.setBackground(new java.awt.Color(240, 240, 240));
        jTextPane1.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jTextPane1.setText("Every observed node is a child of its respective true value node. Observed measurements can be modeled as a continuous distribution whose mean is a linear function of the mean of the parent node, such that :\nchild-node-mean = true-value-parent-node-mean * Coefficient + Offset\nEnter the values of the coefficient and the offset. Also, enter standard deviation to quantify the measurement uncertainty between the true value and the observed value nodes.\n\nHint: If you wish the mean of the child node to be equal to the mean of the parent node, enter 1 for coefficient and 0 for offset.");
        jTextPane1.setAutoscrolls(false);
        jScrollPane2.setViewportView(jTextPane1);

        jLabel1.setText("Node Name:");

        jLabelName.setText("Name Of The Node");

        jTextFieldParentStd.setText("jTextField3");

        jLabel8.setText("Std. Dev:");

        jLabel9.setText("Coefficient:");

        jTextFieldParentCoefficient.setText("jTextField4");

        jTextFieldParentOffset.setText("jTextField5");

        jLabel10.setText("Offset:");

        jButtonUpdate.setText("Update");
        jButtonUpdate.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonUpdateActionPerformed(evt);
            }
        });

        jLabel3.setText("Node Type: Evidence Node");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(248, 248, 248)
                        .addComponent(jButtonUpdate))
                    .addGroup(layout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                            .addGroup(layout.createSequentialGroup()
                                .addGap(174, 174, 174)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addComponent(jLabel10)
                                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                        .addComponent(jLabel8)
                                        .addComponent(jLabel9, javax.swing.GroupLayout.Alignment.TRAILING)))
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                                    .addComponent(jTextFieldParentStd, javax.swing.GroupLayout.PREFERRED_SIZE, 94, javax.swing.GroupLayout.PREFERRED_SIZE)
                                    .addComponent(jTextFieldParentCoefficient)
                                    .addComponent(jTextFieldParentOffset, javax.swing.GroupLayout.PREFERRED_SIZE, 94, javax.swing.GroupLayout.PREFERRED_SIZE)))
                            .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 555, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(jLabel1)
                                .addGap(18, 18, 18)
                                .addComponent(jLabelName, javax.swing.GroupLayout.PREFERRED_SIZE, 111, javax.swing.GroupLayout.PREFERRED_SIZE)
                                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                .addComponent(jLabel3)
                                .addGap(11, 11, 11)))))
                .addContainerGap(31, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(25, 25, 25)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1, javax.swing.GroupLayout.PREFERRED_SIZE, 27, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabelName, javax.swing.GroupLayout.PREFERRED_SIZE, 27, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel3))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 148, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel8)
                    .addComponent(jTextFieldParentStd, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel9)
                    .addComponent(jTextFieldParentCoefficient, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel10)
                    .addComponent(jTextFieldParentOffset, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(11, 11, 11)
                .addComponent(jButtonUpdate)
                .addContainerGap(221, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    public InputNode GetNode()
    {
        return this.node;
    }
    private void jButtonUpdateActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonUpdateActionPerformed
        // TODO add your handling code here:
        if (Validate())
        {

            SetNode();
            FillForm();
            jButtonUpdate.setEnabled(false);
        }
    }//GEN-LAST:event_jButtonUpdateActionPerformed
 private boolean Validate()
    {
        if (isNumeric(jTextFieldParentCoefficient.getText())
                && isNumeric(jTextFieldParentOffset.getText())
                && isNumeric(jTextFieldParentStd.getText()))
        {
            return true;
        }
        else
        {
            JOptionPane.showMessageDialog(this, "Incorrect Input:\n "
                    + "Please Enter numeric values only", 
                    "Error", JOptionPane.ERROR_MESSAGE);
            return false;
        
        }
            
    }
 boolean isNumeric(String str)
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
    public void SetNode()
    {
        node.SetParentStd(Double.parseDouble(jTextFieldParentStd.getText()));
        node.SetParentCoefficient(Double.parseDouble(jTextFieldParentCoefficient.getText()));
        node.SetParentOffset(Double.parseDouble(jTextFieldParentOffset.getText()));
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonUpdate;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel10;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel8;
    private javax.swing.JLabel jLabel9;
    private javax.swing.JLabel jLabelName;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JTextField jTextFieldParentCoefficient;
    private javax.swing.JTextField jTextFieldParentOffset;
    private javax.swing.JTextField jTextFieldParentStd;
    private javax.swing.JTextPane jTextPane1;
    // End of variables declaration//GEN-END:variables
}
