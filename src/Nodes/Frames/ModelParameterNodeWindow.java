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

import MainInterface.Timestep;
import MainInterface.Tree;
import Nodes.ModelParameterNode;
import Nodes.StatisticalDistributions.LinearGaussian;
import Nodes.StatisticalDistributions.StatisticalDistribution;
import Nodes.StatisticalDistributions.TruncatedGaussian;
import Nodes.StatisticalDistributions.UniformDistribution;
import java.awt.CardLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.BorderFactory;
import javax.swing.JOptionPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

/**
 *
 * @author Administrator
 */
public class ModelParameterNodeWindow extends javax.swing.JPanel implements ActionListener {

    /**
     * Creates new form ModelParameterNodeWindow
     */
    private ModelParameterNode node;
    private TruncGaussDistPanel TGPanel;//= new TruncGaussDistPanel(new TruncatedGaussian(0,0,0,0));
    private UniformDistPanel UPanel;//= new UniformDistPanel(new UniformDistribution(0,0));
    private LinearGaussDistPanel LGPanel;

    public ModelParameterNodeWindow(ModelParameterNode n) {
        initComponents();
        this.node = n;
        FillForm();

        //AddActionListener();
        AddChangeListener();
    }

    public void UpdateButtonChangeState(boolean state) {
        jButtonUpdate.setEnabled(state);
    }

    private void AddChangeListener() {
        jTextFieldStd.getDocument().addDocumentListener(new DocumentListener() {
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

    private void FillForm() {
        jScrollPane2.setBorder(BorderFactory.createEmptyBorder());
        StyledDocument doc = jTextPane1.getStyledDocument();
        SimpleAttributeSet center = new SimpleAttributeSet();
        StyleConstants.setAlignment(center, StyleConstants.ALIGN_JUSTIFIED);
        doc.setParagraphAttributes(0, doc.getLength(), center, false);

        this.jLabelName.setText(node.GetName());
        String dist = node.GetDistribtion().getClass().getName();
        if (dist.equals("Nodes.StatisticalDistributions.UniformDistribution")) {
            jRadioButtonUnifDist.setSelected(true);
            ShowUniformDistUI();
        } else if (dist.equals("Nodes.StatisticalDistributions.TruncatedGaussian")) {
            jRadioButtonTruncGaussDist.setSelected(true);
            ShowTruncatedGaussDistUI();
        } else if (dist.equals("Nodes.StatisticalDistributions.LinearGaussian")) {
            jRadioButtonLinearGaussian.setSelected(true);
            ShowLinearGaussUI();
        }
        jTextFieldStd.setText(String.valueOf(node.GetUnscaledStd()));
        jTextFieldScaledStd.setText(String.valueOf(node.GetStd()));
        this.jButtonUpdate.setEnabled(false);
    }

    private void AddActionListener() {
        ActionListener al = new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent ae) {
                jButtonUpdate.setEnabled(true);
                if (jRadioButtonUnifDist.isSelected()) {
                    ShowUniformDistUI();
                } else if (jRadioButtonTruncGaussDist.isSelected()) {
                    ShowTruncatedGaussDistUI();
                }
            }
        };
        jRadioButtonTruncGaussDist.addActionListener(al);
        jRadioButtonUnifDist.addActionListener(al);
    }

    private void ShowUniformDistUI() {

        //Change distribution type of node
        StatisticalDistribution distribution = node.GetDistribtion();
        if (distribution.getClass() != Nodes.StatisticalDistributions.UniformDistribution.class) {
            distribution = new UniformDistribution(distribution);
            //node.SetDistribtion(newDist);
        }

        UPanel = new UniformDistPanel((UniformDistribution) distribution);
        CardLayout layout = (CardLayout) jPanelDistParams.getLayout();
        jPanelDistParams.add(UPanel, "UniformDist");
        layout.show(jPanelDistParams, "UniformDist");
        jPanelDistParams.repaint();
        jPanelDistParams.revalidate();

        //Uniform Dist. does not need these fields
        jTextFieldScaledStd.setText("0.0");
        jTextFieldStd.setText("0.0");
        jTextFieldStd.setEnabled(false);

    }

    private void ShowTruncatedGaussDistUI() {
        StatisticalDistribution dist = node.GetDistribtion();
        //StatisticalDistribution dist = node.GetDistribtion();
        if (dist.getClass() != Nodes.StatisticalDistributions.TruncatedGaussian.class) {
            dist = new TruncatedGaussian(dist);
        }
        TGPanel = new TruncGaussDistPanel((TruncatedGaussian) dist);

        CardLayout layout = (CardLayout) jPanelDistParams.getLayout();
        jPanelDistParams.add(TGPanel, "TruncatedGauss");
        layout.show(jPanelDistParams, "TruncatedGauss");

        jTextFieldStd.setEnabled(true);
        jPanelDistParams.repaint();
        jPanelDistParams.revalidate();
    }

    private void ShowLinearGaussUI() {
        StatisticalDistribution dist = node.GetDistribtion();
        if (dist.getClass() != Nodes.StatisticalDistributions.LinearGaussian.class) {
            dist = new LinearGaussian(dist);
        }

        LGPanel = new LinearGaussDistPanel((LinearGaussian) dist);
        CardLayout layout = (CardLayout) jPanelDistParams.getLayout();
        jPanelDistParams.add(LGPanel, "LinearGauss");
        layout.show(jPanelDistParams, "LinearGauss");

        jTextFieldStd.setEnabled(true);
        jPanelDistParams.repaint();
        jPanelDistParams.revalidate();
    }

    private boolean isNumeric(String str) {
        try {
            double d = Double.parseDouble(str);
        } catch (NumberFormatException nfe) {
            return false;
        }
        return true;
    }

    private void SetNode() {
        if (jRadioButtonTruncGaussDist.isSelected()) {
            node.SetDistribtion(TGPanel.GetDistribution());
        } else if (jRadioButtonUnifDist.isSelected()) {
            node.SetDistribtion(UPanel.GetDistribution());
        } else if (jRadioButtonLinearGaussian.isSelected()) {
            node.SetDistribtion(LGPanel.GetDistribution());
        }
        double timestep = Timestep.TimestepInner.GetTimestep();
        double stepsize = Tree.TreeStatic.GetGeneralSetting().GetStepSize();
        node.SetUnscaledStd(Double.valueOf(jTextFieldStd.getText()));
        node.SetStd(timestep, stepsize);

    }

    private boolean Validate() {
        boolean retVal = false;
        if (!isNumeric(jTextFieldStd.getText())) {
            JOptionPane.showMessageDialog(this, "Incorrect Input:\n "
                    + "Please Enter numeric values only",
                    "Error", JOptionPane.ERROR_MESSAGE);
            retVal = false;

        } else if (jRadioButtonTruncGaussDist.isSelected()) {
            retVal = TGPanel.Validate();
        } else if (jRadioButtonLinearGaussian.isSelected()) {
            retVal = LGPanel.Validate();
        } else if (jRadioButtonUnifDist.isSelected()) {
            retVal = UPanel.Validate();
        }

        return retVal;
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonGroupStatDist = new javax.swing.ButtonGroup();
        jLabel1 = new javax.swing.JLabel();
        jLabel2 = new javax.swing.JLabel();
        jLabelName = new javax.swing.JLabel();
        jRadioButtonUnifDist = new javax.swing.JRadioButton();
        jLabel3 = new javax.swing.JLabel();
        jRadioButtonTruncGaussDist = new javax.swing.JRadioButton();
        jRadioButton3 = new javax.swing.JRadioButton();
        jRadioButtonLinearGaussian = new javax.swing.JRadioButton();
        jLabel4 = new javax.swing.JLabel();
        jPanelDistParams = new javax.swing.JPanel();
        jLabel5 = new javax.swing.JLabel();
        jTextFieldStd = new javax.swing.JTextField();
        jButtonUpdate = new javax.swing.JButton();
        jLabel6 = new javax.swing.JLabel();
        jTextFieldScaledStd = new javax.swing.JTextField();
        jScrollPane2 = new javax.swing.JScrollPane();
        jTextPane1 = new javax.swing.JTextPane();

        setBorder(javax.swing.BorderFactory.createEtchedBorder());
        setMinimumSize(new java.awt.Dimension(600, 560));
        setPreferredSize(new java.awt.Dimension(600, 560));

        jLabel1.setLabelFor(jLabelName);
        jLabel1.setText("Node Name:");

        jLabel2.setText("Node Type: Model Parameter");

        jLabelName.setText("Name of the Node");

        buttonGroupStatDist.add(jRadioButtonUnifDist);
        jRadioButtonUnifDist.setText("Uniform Distribution");
        jRadioButtonUnifDist.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jRadioButtonUnifDistItemStateChanged(evt);
            }
        });

        jLabel3.setText("Select the type of statistical distribution of the node:");

        buttonGroupStatDist.add(jRadioButtonTruncGaussDist);
        jRadioButtonTruncGaussDist.setText("Truncated Gaussian Distribution");
        jRadioButtonTruncGaussDist.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jRadioButtonTruncGaussDistItemStateChanged(evt);
            }
        });

        buttonGroupStatDist.add(jRadioButton3);
        jRadioButton3.setText("Tabulated Bnode");
        jRadioButton3.setEnabled(false);

        buttonGroupStatDist.add(jRadioButtonLinearGaussian);
        jRadioButtonLinearGaussian.setText("Linear Gaussian");
        jRadioButtonLinearGaussian.addItemListener(new java.awt.event.ItemListener() {
            public void itemStateChanged(java.awt.event.ItemEvent evt) {
                jRadioButtonLinearGaussianItemStateChanged(evt);
            }
        });

        jLabel4.setText("Parameters for the initial value of the node:");

        jPanelDistParams.setPreferredSize(new java.awt.Dimension(567, 100));
        jPanelDistParams.setLayout(new java.awt.CardLayout());

        jLabel5.setLabelFor(jTextFieldStd);
        jLabel5.setText("Std. Dev:");

        jTextFieldStd.setText("0.00");

        jButtonUpdate.setText("Update");
        jButtonUpdate.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonUpdateActionPerformed(evt);
            }
        });

        jLabel6.setText("Std. Dev Scaled:");

        jTextFieldScaledStd.setText("jTextField1");
        jTextFieldScaledStd.setEnabled(false);

        jScrollPane2.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

        jTextPane1.setEditable(false);
        jTextPane1.setBackground(new java.awt.Color(240, 240, 240));
        jTextPane1.setBorder(BorderFactory.createEmptyBorder());
        jTextPane1.setText("Model Parameters vary in each time step by including a conditional dependency on its value in the previous time slice. Enter the standard deviation to represent the deviation from the value of the node at the previous time slice.");
        jTextPane1.setAutoscrolls(false);
        jScrollPane2.setViewportView(jTextPane1);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabel1)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jLabelName)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                        .addComponent(jLabel2)
                        .addGap(20, 20, 20))
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                            .addComponent(jLabel3)
                            .addGroup(layout.createSequentialGroup()
                                .addGap(29, 29, 29)
                                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                                    .addComponent(jRadioButtonLinearGaussian)
                                    .addComponent(jRadioButton3)
                                    .addComponent(jRadioButtonTruncGaussDist)
                                    .addComponent(jRadioButtonUnifDist)))
                            .addComponent(jPanelDistParams, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                            .addComponent(jLabel4)
                            .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 554, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addGap(0, 19, Short.MAX_VALUE))))
            .addGroup(layout.createSequentialGroup()
                .addGap(51, 51, 51)
                .addComponent(jLabel5)
                .addGap(18, 18, 18)
                .addComponent(jTextFieldStd, javax.swing.GroupLayout.PREFERRED_SIZE, 74, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(54, 54, 54)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(jButtonUpdate)
                        .addGap(191, 191, 191))
                    .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                        .addComponent(jLabel6)
                        .addGap(18, 18, 18)
                        .addComponent(jTextFieldScaledStd, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jLabel1)
                    .addComponent(jLabel2)
                    .addComponent(jLabelName))
                .addGap(32, 32, 32)
                .addComponent(jLabel3)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jRadioButtonUnifDist)
                .addGap(3, 3, 3)
                .addComponent(jRadioButtonTruncGaussDist)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jRadioButton3)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jRadioButtonLinearGaussian)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                .addComponent(jLabel4)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelDistParams, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(27, 27, 27)
                .addComponent(jScrollPane2, javax.swing.GroupLayout.PREFERRED_SIZE, 68, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(layout.createSequentialGroup()
                        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel6)
                            .addComponent(jTextFieldScaledStd, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jButtonUpdate))
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel5)
                        .addComponent(jTextFieldStd, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addContainerGap(88, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents

    private void jButtonUpdateActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonUpdateActionPerformed
        // TODO add your handling code here:
        if (Validate()) {

            SetNode();
            FillForm();
            jButtonUpdate.setEnabled(false);
        }
    }//GEN-LAST:event_jButtonUpdateActionPerformed

    private void jRadioButtonTruncGaussDistItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_jRadioButtonTruncGaussDistItemStateChanged
        // TODO add your handling code here:
        DistTypeChanged();
    }//GEN-LAST:event_jRadioButtonTruncGaussDistItemStateChanged

    private void jRadioButtonUnifDistItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_jRadioButtonUnifDistItemStateChanged
        // TODO add your handling code here:
        DistTypeChanged();
    }//GEN-LAST:event_jRadioButtonUnifDistItemStateChanged

    private void jRadioButtonLinearGaussianItemStateChanged(java.awt.event.ItemEvent evt) {//GEN-FIRST:event_jRadioButtonLinearGaussianItemStateChanged
        // TODO add your handling code here:
        DistTypeChanged();
    }//GEN-LAST:event_jRadioButtonLinearGaussianItemStateChanged

    private void DistTypeChanged() {
        jButtonUpdate.setEnabled(true);
        if (jRadioButtonUnifDist.isSelected()) {
            ShowUniformDistUI();
        } else if (jRadioButtonTruncGaussDist.isSelected()) {
            ShowTruncatedGaussDistUI();
        } else if (jRadioButtonLinearGaussian.isSelected()) {
            ShowLinearGaussUI();
        }
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroupStatDist;
    private javax.swing.JButton jButtonUpdate;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabel5;
    private javax.swing.JLabel jLabel6;
    private javax.swing.JLabel jLabelName;
    private javax.swing.JPanel jPanelDistParams;
    private javax.swing.JRadioButton jRadioButton3;
    private javax.swing.JRadioButton jRadioButtonLinearGaussian;
    private javax.swing.JRadioButton jRadioButtonTruncGaussDist;
    private javax.swing.JRadioButton jRadioButtonUnifDist;
    private javax.swing.JScrollPane jScrollPane2;
    private javax.swing.JTextField jTextFieldScaledStd;
    private javax.swing.JTextField jTextFieldStd;
    private javax.swing.JTextPane jTextPane1;
    // End of variables declaration//GEN-END:variables

    @Override
    public void actionPerformed(ActionEvent ae) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
