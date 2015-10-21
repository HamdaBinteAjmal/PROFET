/*
Copyright (C) 2010 Alex Billingsley, email@alexbillingsley.co.uk
www.dragmath.bham.ac.uk
 
 This file is part of DragMath.

    DragMath is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    DragMath is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with DragMath. If not, see <http://www.gnu.org/licenses/>.

 */

package EquationEditor.Display;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.awt.Cursor;
/**
 *
 * @author  Alex Billingsley
 */
public class AboutFrame extends javax.swing.JFrame {
    
    private java.applet.AppletContext appletContext;
    
    /** Creates new form NewJFrame */
    public AboutFrame(java.applet.AppletContext appletContext) {
        this.appletContext=appletContext;
        initComponents();
    }
    
    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel1 = new javax.swing.JPanel();
        jLabel2 = new javax.swing.JLabel();
        jLabel1 = new javax.swing.JLabel();
        jLabel3 = new javax.swing.JLabel();
        jLabel4 = new javax.swing.JLabel();
        jButton1 = new javax.swing.JButton();
        jLabelAlex = new javax.swing.JLabel();
        jLabelChris = new javax.swing.JLabel();

        setDefaultCloseOperation(javax.swing.WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("About DragMath");
        setBackground(new java.awt.Color(255, 255, 255));
        setResizable(false);
        getContentPane().setLayout(new org.netbeans.lib.awtextra.AbsoluteLayout());

        jPanel1.setBackground(new java.awt.Color(255, 255, 255));

        jLabel2.setFont(new java.awt.Font("Verdana", 1, 14)); // NOI18N
        jLabel2.setForeground(new java.awt.Color(153, 153, 153));
        jLabel2.setText("version 0.7.9");

        jLabel1.setFont(new java.awt.Font("Arial", 1, 36)); // NOI18N
        jLabel1.setForeground(new java.awt.Color(0, 0, 102));
        jLabel1.setText("DragMath");

        jLabel3.setFont(new java.awt.Font("Arial", 0, 14)); // NOI18N
        jLabel3.setText("Developed by:");

        jLabel4.setFont(new java.awt.Font("Arial", 1, 18)); // NOI18N
        jLabel4.setForeground(new java.awt.Color(0, 0, 102));
        jLabel4.setText("www.dragmath.bham.ac.uk");
        jLabel4.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jLabel4MouseClicked(evt);
            }
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                jLabel4MouseEntered(evt);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                jLabel4MouseExited(evt);
            }
        });

        jButton1.setText("OK");
        jButton1.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jButton1MouseClicked(evt);
            }
        });

        jLabelAlex.setFont(new java.awt.Font("Arial", 0, 14)); // NOI18N
        jLabelAlex.setText("- Alex Billingsley");
        jLabelAlex.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jLabelAlexMouseClicked(evt);
            }
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                jLabelAlexMouseEntered(evt);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                jLabelAlexMouseExited(evt);
            }
        });

        jLabelChris.setFont(new java.awt.Font("Arial", 0, 14)); // NOI18N
        jLabelChris.setText("- Chris Sangwin");
        jLabelChris.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jLabelChrisMouseClicked(evt);
            }
            public void mouseEntered(java.awt.event.MouseEvent evt) {
                jLabelChrisMouseEntered(evt);
            }
            public void mouseExited(java.awt.event.MouseEvent evt) {
                jLabelChrisMouseExited(evt);
            }
        });

        org.jdesktop.layout.GroupLayout jPanel1Layout = new org.jdesktop.layout.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(jPanel1Layout.createSequentialGroup()
                .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                    .add(jPanel1Layout.createSequentialGroup()
                        .add(34, 34, 34)
                        .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                            .add(jLabel2)
                            .add(jLabel1)
                            .add(jLabel3)
                            .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.TRAILING)
                                .add(jButton1, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE, 79, org.jdesktop.layout.GroupLayout.PREFERRED_SIZE)
                                .add(jLabel4))))
                    .add(jPanel1Layout.createSequentialGroup()
                        .add(53, 53, 53)
                        .add(jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                            .add(jLabelChris)
                            .add(jLabelAlex))))
                .addContainerGap(18, Short.MAX_VALUE))
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(jPanel1Layout.createSequentialGroup()
                .add(36, 36, 36)
                .add(jLabel1)
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jLabel2)
                .add(30, 30, 30)
                .add(jLabel3)
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jLabelAlex)
                .addPreferredGap(org.jdesktop.layout.LayoutStyle.RELATED)
                .add(jLabelChris)
                .add(23, 23, 23)
                .add(jLabel4)
                .add(31, 31, 31)
                .add(jButton1)
                .addContainerGap(20, Short.MAX_VALUE))
        );

        getContentPane().add(jPanel1, new org.netbeans.lib.awtextra.AbsoluteConstraints(0, 0, -1, -1));

        pack();
    }// </editor-fold>//GEN-END:initComponents

    private void jLabel4MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jLabel4MouseClicked
        try {
            appletContext.showDocument(new java.net.URL("http://www.dragmath.bham.ac.uk"), "_blank");
        } catch (Exception ex) {
        }
    }//GEN-LAST:event_jLabel4MouseClicked

    private void jLabel4MouseEntered(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jLabel4MouseEntered
        setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
    }//GEN-LAST:event_jLabel4MouseEntered

    private void jLabel4MouseExited(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jLabel4MouseExited
        setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }//GEN-LAST:event_jLabel4MouseExited

    private void jButton1MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButton1MouseClicked
        this.dispose();
    }//GEN-LAST:event_jButton1MouseClicked

    private void jLabelAlexMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jLabelAlexMouseClicked
        try {
            appletContext.showDocument(new java.net.URL("mailto:email@abillingsley.co.uk?subject=DragMath"), "_blank");
        } catch (Exception ex) {
        }
    }//GEN-LAST:event_jLabelAlexMouseClicked

    private void jLabelAlexMouseEntered(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jLabelAlexMouseEntered
        setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
    }//GEN-LAST:event_jLabelAlexMouseEntered

    private void jLabelAlexMouseExited(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jLabelAlexMouseExited
        setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }//GEN-LAST:event_jLabelAlexMouseExited

    private void jLabelChrisMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jLabelChrisMouseClicked
        try {
            appletContext.showDocument(new java.net.URL("mailto:C.J.Sangwin@bham.ac.uk?subject=DragMath"), "_blank");
        } catch (Exception ex) {
        }
    }//GEN-LAST:event_jLabelChrisMouseClicked

    private void jLabelChrisMouseEntered(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jLabelChrisMouseEntered
        setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
    }//GEN-LAST:event_jLabelChrisMouseEntered

    private void jLabelChrisMouseExited(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jLabelChrisMouseExited
        setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }//GEN-LAST:event_jLabelChrisMouseExited
                                            
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton1;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabel4;
    private javax.swing.JLabel jLabelAlex;
    private javax.swing.JLabel jLabelChris;
    private javax.swing.JPanel jPanel1;
    // End of variables declaration//GEN-END:variables
    
}
