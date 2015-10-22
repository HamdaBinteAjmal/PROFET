/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Nodes.Frames;

import Nodes.ModelVariableNode;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 *
 * @author Administrator
 */
public class ModelVariableNodeWindow extends javax.swing.JPanel {

    /**
     * Creates new form ModelVariableNodeWindow
     */
    private ModelVariableNode node;
    public ModelVariableNodeWindow(ModelVariableNode node) {
        initComponents();
        this.node = node;
        AddChangeListener(jTextFieldInitialValue);
        FillForm();
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
        
        this.jTextFieldInitialValue.setText(Double.toString(node.GetInitialValue()));
        this.jLabelNodeName.setText(node.GetName());
        this.jCheckBoxEvidenceNode.setSelected(node.alsoAnEvidenceNode);
        this.jButtonUpdate.setEnabled(false);
                
    }
    private void SetNode()
    {
         node.SetInitialValue(Double.parseDouble(jTextFieldInitialValue.getText()));
         node.AddAnEvidenceNode(jCheckBoxEvidenceNode.isSelected());
        
    }
    
    public ModelVariableNode GetNode()
    {
        SetNode();
        return this.node;
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
        jTextFieldInitialValue = new javax.swing.JTextField();
        jCheckBoxEvidenceNode = new javax.swing.JCheckBox();
        jButtonUpdate = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jLabelNodeName = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();

        setBorder(javax.swing.BorderFactory.createEtchedBorder());
        setMinimumSize(new java.awt.Dimension(600, 560));
        setPreferredSize(new java.awt.Dimension(600, 560));

        jLabel1.setText("Enter Initial Value:");

        jTextFieldInitialValue.setText("jTextField1");

        jCheckBoxEvidenceNode.setText("Add a corresponding evidence Node");
        jCheckBoxEvidenceNode.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jCheckBoxEvidenceNodeActionPerformed(evt);
            }
        });

        jButtonUpdate.setText("Update");
        jButtonUpdate.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonUpdateActionPerformed(evt);
            }
        });

        jLabel2.setText("Node Name:");

        jLabelNodeName.setText("Node Name");

        jLabel3.setText("Node Type: Model Variable");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel2)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabelNodeName)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addComponent(jLabel3)
                .addGap(18, 18, 18))
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGap(205, 205, 205)
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
                            .addComponent(jCheckBoxEvidenceNode)
                            .addGroup(layout.createSequentialGroup()
                                .addComponent(jLabel1)
                                .addGap(18, 18, 18)
                                .addComponent(jTextFieldInitialValue, javax.swing.GroupLayout.PREFERRED_SIZE, 94, javax.swing.GroupLayout.PREFERRED_SIZE))))
                    .addGroup(layout.createSequentialGroup()
                        .addGap(266, 266, 266)
                        .addComponent(jButtonUpdate)))
                .addContainerGap(191, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(14, 14, 14)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel2)
                    .addComponent(jLabelNodeName)
                    .addComponent(jLabel3))
                .addGap(47, 47, 47)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jTextFieldInitialValue, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addGap(18, 18, 18)
                .addComponent(jCheckBoxEvidenceNode)
                .addGap(18, 18, 18)
                .addComponent(jButtonUpdate)
                .addContainerGap(379, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    private void jButtonUpdateActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonUpdateActionPerformed
        // TODO add your handling code here:
       SetNode();
       jButtonUpdate.setEnabled(false);
    }//GEN-LAST:event_jButtonUpdateActionPerformed

    private void jCheckBoxEvidenceNodeActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jCheckBoxEvidenceNodeActionPerformed
        // TODO add your handling code here:
        jButtonUpdate.setEnabled(true);
    }//GEN-LAST:event_jCheckBoxEvidenceNodeActionPerformed


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonUpdate;
    private javax.swing.JCheckBox jCheckBoxEvidenceNode;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabelNodeName;
    private javax.swing.JTextField jTextFieldInitialValue;
    // End of variables declaration//GEN-END:variables
}