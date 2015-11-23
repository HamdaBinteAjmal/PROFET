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
import Nodes.DBNNode;
import Nodes.InputNode;
import Nodes.ModelParameterNode;
import Nodes.ModelVariableNode;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import javax.swing.AbstractButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JToggleButton;

/**
 *
 * @author Administrator
 */
class OutputNodePanel extends javax.swing.JPanel implements ItemListener
{
   // private JLabel name;
    private JCheckBox outputRequired;
    DBNNode dbnNode;
    
    OutputNodePanel(InputNode node)
    {
        dbnNode = node;
        outputRequired = new JCheckBox(node.toString());
        outputRequired.setSelected(node.IsOutputRequired());
        PopulatePanel();
    }
    OutputNodePanel(ModelVariableNode node)
    {
        dbnNode = node;
        outputRequired = new JCheckBox(node.toString());
        outputRequired.setSelected(node.IsOutputRequired());
        PopulatePanel();
    }
    OutputNodePanel(ModelParameterNode node)
    {
        dbnNode = node;
       // name = new JLabel(node.toString());
        outputRequired = new JCheckBox(node.toString());
        outputRequired.setSelected(node.IsOutputRequired());
        PopulatePanel();
    }
    private void PopulatePanel()
    {
        this.removeAll();
        GridLayout outputLayout = new GridLayout(0,1);
        outputLayout.setHgap(5);   
        this.setLayout(outputLayout);
        outputRequired.addItemListener(this);
       // outputRequired.addActionListener(this);
        //this.add(name);
        this.add(outputRequired);
    }

    public void itemStateChanged(ItemEvent e)
        {
            dbnNode.SetOutputRequired(outputRequired.isSelected());
        }
//    @Override
//    public void actionPerformed(ActionEvent ae) 
//    {
//        dbnNode.SetOutputRequired(outputRequired.isSelected());
//    }
    public void SetSelected(boolean select)
    {
        outputRequired.setSelected(select);
    }
}
public class OutputNodes  {

    /**
     * Creates new form OutputNodes
     */
    
    public static class OutputNodesInner extends javax.swing.JPanel  
    {
        private JLabel titleLabel = new JLabel();        
        private JToggleButton selectAll;
        private JPanel topPanel;
        JPanel centerPanel = new JPanel();

        public OutputNodesInner() 
        {
            initComponents();
            selectAll = new JToggleButton("Deselect All");
            selectAll.setSelected(true);
            FlowLayout fl = new FlowLayout();
            fl.setAlignment(FlowLayout.LEFT);
            fl.setHgap(30);
            topPanel = new JPanel();
            topPanel.setLayout(fl);
            
            //BorderLayout layout = new BorderLayout(10,10);
            PopulateUI();
             AddActionListener();
            //selectAll.addItemListener(this);
        }
        
        private void AddActionListener()
        {
            ActionListener actionListener = new ActionListener() 
            {
            @Override
            public void actionPerformed(ActionEvent actionEvent) 
            {
          
                AbstractButton abstractButton = (AbstractButton) actionEvent.getSource();
                boolean selected = abstractButton.getModel().isSelected();
                if (selected)
                {
                    selectAll.setText("Deselect All");
                }
                else
                {
                    selectAll.setText("Select All");
                }
                SelectAll(selected);
            }
        };
            selectAll.addActionListener(actionListener);
        }
         
        private void SelectAll(boolean select)
        {
            Component[] components = centerPanel.getComponents();
            for (Component comp : components)
            {
                if (comp instanceof OutputNodePanel)
                {
                    OutputNodePanel p = (OutputNodePanel)comp;
                    p.SetSelected(select);
                }
            }
            
        }
        public void Update()
        {
            PopulateUI();
        }
        private void PopulateUI()
        {           
            this.removeAll();  
            centerPanel.removeAll();
            topPanel.removeAll();
            //evidenceList.clear();
            this.setLayout(new BorderLayout(1, 10));
            titleLabel = new JLabel("Select the nodes which need output");
            topPanel.add(titleLabel);
            topPanel.add(selectAll);
            
            this.add(topPanel, BorderLayout.PAGE_START);
        
            
            GridLayout layout = new GridLayout(15, 3);
            layout.setVgap(10);
            layout.setHgap(10);
            centerPanel.setLayout(layout);
            //MakeUpdateButton();
            for (InputNode node : Tree.TreeStatic.GetAllInputNodes())
            {
                OutputNodePanel panel = new OutputNodePanel(node);
                centerPanel.add(panel);   
            }
            for (ModelParameterNode node : Tree.TreeStatic.GetAllModelParameterNodes())
            {
                OutputNodePanel panel = new OutputNodePanel(node);
                centerPanel.add(panel);
            }
            for (ModelVariableNode node : Tree.TreeStatic.GetAllModelVariableNodes())
            {
                OutputNodePanel panel = new OutputNodePanel(node);
                centerPanel.add(panel);
            }
            this.add(centerPanel, BorderLayout.CENTER);
            //this.add(jButtonUpdate, BorderLayout.PAGE_END);
        
            this.repaint();
            this.setVisible(true);
    }
        
        @Override
    public String toString()
    {
        return "Select Output";
    }
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

    }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables
}
