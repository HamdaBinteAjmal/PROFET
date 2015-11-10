/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package RunInference;

import MainInterface.Tree;
import Nodes.InputNode;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;

/**
 *
 * @author Administrator
 */


public class EvidenceUploadWindow {
    public static class EvidenceUploadWindowInner extends javax.swing.JPanel {
    /**
     * Creates new form EvidenceUploadWindow
     */
    
        public EvidenceUploadWindowInner() {
        initComponents();
        PopulateUI();
        }
    
        private static ArrayList<EvidencePanel> evidenceList = new ArrayList<>();
        private static JButton jButtonUpdate;
        private static JLabel titleLabel;
        
    

        @Override
        public String toString()
        {
            return "Attach Evidence";
        }
//        private void UpdateEvidence()
//        {
//            for (EvidencePanel panel : evidenceList)
//            {
//              //  Tree.TreeStatic.SetEvidenceForInputNode(panel.GetNode());
//            }
//           
//        }
//        private void MakeUpdateButton()
//        {
//            jButtonUpdate = new JButton("Update");
//            //Add action listener to button
//            jButtonUpdate.addActionListener(new ActionListener() {
//        
//            @Override
//            public void actionPerformed(ActionEvent e)
//            {
//                //if(Validate())
//                //{
//                    //UpdateEvidence();
//                    jButtonUpdate.setEnabled(false);
//                //}
//            }
//        });      
//        jButtonUpdate.setEnabled(false);
        //this.add(button);
        //this.repaint();
      // return button;
//    }
        
        private void PopulateUI()
        {
            this.removeAll();     
            evidenceList.clear();
            this.setLayout(new BorderLayout(10, 10));
            titleLabel = new JLabel("Add the data file in .csv format for the following evidence nodes.");
            this.add(titleLabel, BorderLayout.PAGE_START);
        
            JPanel centerPanel = new JPanel();
            GridLayout layout = new GridLayout(0,1);
            layout.setVgap(10);
            layout.setHgap(10);
            centerPanel.setLayout(layout);
           // MakeUpdateButton();
            for (InputNode node : Tree.TreeStatic.GetAllInputNodes())
            {
                EvidencePanel evidencePanel = new EvidencePanel(node);
                centerPanel.add(evidencePanel);   
                evidenceList.add(evidencePanel);
            }
        
            this.add(centerPanel, BorderLayout.CENTER);
            //this.add(jButtonUpdate, BorderLayout.PAGE_END);
        
            this.repaint();
            this.setVisible(true);
        } // return button;
    
    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 600, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 560, Short.MAX_VALUE)
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables

        public void Update()
        {
            PopulateUI();
        }
        
//        public static void ResetUpdateButton()
//        {
//            jButtonUpdate.setEnabled(true);
//        }
        
}
}
class EvidencePanel extends javax.swing.JPanel implements ActionListener
    {
        InputNode node;
        JLabel titleLabel;
        JRadioButton instantaneos = new JRadioButton("Instantaneous");
        JRadioButton continous = new JRadioButton("Continous");
        ButtonGroup group = new ButtonGroup();
        JButton browse;
        JLabel fileLabel;
       
        public InputNode GetNode()
        {
            return this.node;
        }
        public EvidencePanel(InputNode node)
        {
            this.node = node;
            this.removeAll();
            GridLayout evidenceLayout = new GridLayout(0,5);
            evidenceLayout.setHgap(15);   
            this.setLayout(evidenceLayout);
            JLabel nodename = new JLabel(node.toString());
            this.add(nodename);
           
            browse = new JButton("Browse");
            this.add(browse);
            browse.addActionListener(this); 
            
           
            instantaneos = new JRadioButton("Instantaneous");
            continous = new JRadioButton("Continous");
            group = new ButtonGroup();
            group.add(continous);
            group.add(instantaneos);
           if (node.GetEvidence().isContinous())
            {
                continous.setSelected(true);
                instantaneos.setSelected(false);
            }
            else
            {
                continous.setSelected(false);
                instantaneos.setSelected(true);
            }
             
            this.add(continous);
            this.add(instantaneos);           
            
            fileLabel = new JLabel();
            
            File file = node.GetEvidence().GetFile();
            if (file == null)
            {
                fileLabel.setText("Not Uploaded");
                fileLabel.setForeground(Color.red);
            }
            else
            {
                fileLabel.setText(file.getName());
                fileLabel.setForeground(Color.decode("#006400"));
            }
            this.add(fileLabel);
            continous.addActionListener(this);
            instantaneos.addActionListener(this);
            this.repaint();
            this.setVisible(true);
                        
        }
         @Override
        public void actionPerformed(ActionEvent ae) {
            
            if (ae.getSource() == browse)
            {
                JFileChooser fc = new JFileChooser();
                FileFilter filter = new FileNameExtensionFilter("csv file","csv");
                fc.setFileFilter(filter);
                int returnVal = fc.showOpenDialog(this);
            
                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    File file = fc.getSelectedFile();
                    node.GetEvidence().SetFile(file);
                fileLabel.setText(file.getName());
                fileLabel.setForeground(Color.decode("#006400"));
                this.repaint();
                this.setVisible(true);
                }
                //EvidenceUploadWindow.EvidenceUploadWindowInner.ResetUpdateButton();
            }
            if (ae.getSource() == continous || ae.getSource() == instantaneos)
            {
                if(continous.isSelected())
                    node.GetEvidence().SetContinous();
                else
                    node.GetEvidence().SetInstantaneous();
                //EvidenceUploadWindow.EvidenceUploadWindowInner.ResetUpdateButton();
            }
    }
        
        
    }
