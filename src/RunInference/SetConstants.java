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

import MainInterface.Tree;
import Nodes.Constant;
import Nodes.GlobalLists;
import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 *
 * @author Administrator
 */
class ConstantUI{
    public JTextField textField;
    public JLabel label;
    Constant constant;
    JPanel panel = new JPanel();
    public ConstantUI(Constant c)
    {
        constant = c;
        textField = new JTextField(String.valueOf(c.GetValue()));
        label = new JLabel(c.toString() + " :");
        //textField.setSize(25);
        GridLayout layout = new GridLayout(1, 2);
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.weightx = 1;
        //layout.setHgap(20);
        panel.setLayout(layout);
        panel.add(label);
        panel.add(textField, constraints);
        SetConstants.SetConstantsInner.AddChangeListener(textField);
        
    }
    public Constant GetConstant()
    {
        double value = Double.valueOf(textField.getText());
        constant.SetValue(value);
        return constant;
    }
    public JPanel GetPanel()
    {
        return panel;
    }
}

public class SetConstants {
    public static class SetConstantsInner extends javax.swing.JPanel {
    /**
     * Creates new form SetConstants
     */
    private ArrayList<ConstantUI> constantsUIs = new ArrayList<>();
    private static JButton jButtonUpdate;
    public SetConstantsInner() {
        initComponents();
        BorderLayout layout = new BorderLayout(1,10);
        this.setLayout(layout);
        
        PopulateUI();
    }
    public void Update()
    {
        PopulateUI();
    }
    private void PopulateUI()
    {        
        this.removeAll();
        constantsUIs.clear();
        JPanel fields = new JPanel();
        
        GridLayout layout  = new GridLayout(15,0);
        layout.setVgap(10);
        layout.setHgap(10);
        fields.setLayout(layout);
      //  GridBagConstraints constraints = new GridBagConstraints();
       // constraints.weightx = 1;
        SetLists();
        for (ConstantUI cui : constantsUIs)
        {
           fields.add(cui.GetPanel());
        }
        
        
        JLabel mainLabel = new JLabel("Set the values of the constants:");
        this.add(mainLabel, BorderLayout.PAGE_START);
        this.add(fields, BorderLayout.CENTER);
        MakeUpdateButton();
        JPanel buttonPanel = new  JPanel();
        buttonPanel.add(jButtonUpdate);
        this.add(buttonPanel, BorderLayout.PAGE_END);
        this.repaint();
        this.setVisible(true);
    }
    
    
    private void SetLists()
    {
        ArrayList<Constant> list = Tree.TreeStatic.GetAllConstants();
        for (Constant c : list)
        {
            ConstantUI CUI = new ConstantUI(c);
            constantsUIs.add(CUI);
        }
    }

    public static void AddChangeListener(JTextField field)
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
    private void UpdateConstants()
    {
        HashMap<Constant, Integer> UpdatedConstants = new HashMap<>();
         for (ConstantUI cui : constantsUIs)
         {
            Constant updatedC = cui.GetConstant();
             Iterator it = GlobalLists.Constants.entrySet().iterator();
            while (it.hasNext())
            {
                Map.Entry pair = (Map.Entry)it.next();
                Constant c = (Constant)pair.getKey();
                int count = (Integer)pair.getValue();
                
                if (c.toString().equals(updatedC.toString()))
                {
                    UpdatedConstants.put(updatedC, count);
                    break;
                }
            }
         }
         
         GlobalLists.Constants.clear();
         GlobalLists.Constants = UpdatedConstants;
       
    }
    private void MakeUpdateButton()
    {
       jButtonUpdate = new JButton("Update");
        //Add action listener to button
        jButtonUpdate.addActionListener(new ActionListener() {
        
            @Override
            public void actionPerformed(ActionEvent e)
            {
                if(Validate())
                {
                    UpdateConstants();
                    jButtonUpdate.setEnabled(false);
                }
            }
        });      
        jButtonUpdate.setEnabled(false);
        //this.add(button);
        //this.repaint();
      // return button;
    }
      private boolean Validate()
    {
        boolean retVal = true;
        for (ConstantUI cui : constantsUIs)
        {
            if (!isNumeric(cui.textField.getText()))
            {
                
                retVal = false;
                break;
            }
                    
        }
        if (!retVal)
        {
            JOptionPane.showMessageDialog(this, "Incorrect Input:\n "
                    + "Please Enter numeric values only", 
                    "Error", JOptionPane.ERROR_MESSAGE);
        }
        return retVal;
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
    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        setMinimumSize(new java.awt.Dimension(600, 560));

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

    @Override
    public String toString()
    {
        return "Set Constant Values";
    }
}}