/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package RunInference;

import MainInterface.Tree;
import Nodes.ModelVariableNode;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

/**
 *
 * @author Administrator
 */
class ToleranceNodePanel extends javax.swing.JPanel implements ActionListener
{
   // private JLabel name;
    private JCheckBox toleranceImposed;
   
    ModelVariableNode node;

    ToleranceNodePanel(ModelVariableNode node)
    {
        this.node = node;
        //name = new JLabel(node.toString());
        toleranceImposed = new JCheckBox("Delta-" + node.toString());
        toleranceImposed.setSelected(node.ImposeTolerance());        
        PopulatePanel();
    }
    private int toleranceNodeCount()
    {
        int count = 0;
          for (ModelVariableNode node : Tree.TreeStatic.GetAllModelVariableNodes())
        {
            if (node.ImposeTolerance())
            {
                count++;
            }
        }
          return count;
    }
    private void PopulatePanel()
    {
        this.removeAll();
        GridLayout outputLayout = new GridLayout(0,2);
        outputLayout.setHgap(15);   
        this.setLayout(outputLayout);
        toleranceImposed.addActionListener(this);
        //this.add(name);
        this.add(toleranceImposed);
    }

    @Override
    public void actionPerformed(ActionEvent ae) 
    {
        if (!toleranceImposed.isSelected())
        {
            if (toleranceNodeCount() <= 1)
            {
                toleranceImposed.setSelected(true);
            }
        }
        node.ImposeTolerance(toleranceImposed.isSelected());
    }
    
}
public class ToleranceWindow  {
    
public static class ToleranceWindowInner extends javax.swing.JPanel{
    /**
     * Creates new form ToleranceWindow
     */
    //private static double tolerance = 0.01;
    private static Tolerance tolerance = new Tolerance();
  //  private static DefaultListModel<String> ToleranceListModel = new DefaultListModel<>();
    private JLabel toleranceLabel = new JLabel("Set the tolerance to limit the numeric error introduced at each time step");
    private JTextField jTextFieldTolerance = new JTextField();
    private JPanel TolerancePanel = new JPanel();
     JLabel nodesLabel = new JLabel("Select at least one delta node to meet the tolerance criteria");
    private JButton jButtonUpdate = new JButton("Update");
    JPanel centerPanel = new JPanel();
    
    public ToleranceWindowInner() {
        initComponents();  
        tolerance = new Tolerance();
         this.setLayout(new BorderLayout(/*1, 10*/));
        TolerancePanel = new JPanel();        
        
       // jTextFieldTolerance = new JTextField();     
        
        jTextFieldTolerance.setPreferredSize(new Dimension(50, 10)); 
        TolerancePanel.setLayout(new BoxLayout(TolerancePanel, BoxLayout.LINE_AXIS));
       // TolerancePanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10));
        TolerancePanel.add(toleranceLabel);
        
       TolerancePanel.add(Box.createRigidArea(new Dimension(10, 0)));
        TolerancePanel.add(jTextFieldTolerance);
        TolerancePanel.add(Box.createRigidArea(new Dimension(10, 0)));
        TolerancePanel.add(jButtonUpdate);
        jButtonUpdate.setEnabled(false);
        
        centerPanel = new JPanel();
        GridLayout layout = new GridLayout(15, 0);
        layout.setVgap(10);
        layout.setHgap(10);
        centerPanel.setLayout(layout);
       
      //  centerPanel.add(nodesLabel);
        this.add(centerPanel, BorderLayout.CENTER);
      //  PopulateUI();
        AddChangeListener();
        
    }
     public static void SetTolerance(Tolerance t) {
        tolerance.SetTolerance(t.GetTolerance());
       // PopulateUI();
       
    }
    private void AddTolerancePanel()
    {
        
        jTextFieldTolerance.setText(String.valueOf(tolerance.GetTolerance()));
        this.add(TolerancePanel, BorderLayout.PAGE_START);
    }
    private void AddNodePanel()
    {
        
       centerPanel.removeAll();
       centerPanel.add(nodesLabel);
        for (ModelVariableNode node : Tree.TreeStatic.GetAllModelVariableNodes())
        {
            ToleranceNodePanel panel = new ToleranceNodePanel(node);
            
            centerPanel.add(panel);
        }
        centerPanel.repaint();
        centerPanel.setVisible(true);
        
    }
   
    public void Update()
    {
        PopulateUI();
    }
    private void PopulateUI()
    { 
        AddTolerancePanel(); 
        AddNodePanel();
        jButtonUpdate.setEnabled(false);
        this.repaint();
        this.setVisible(true);
    }
    @Override
    public String toString()
    {
        return "Set Tolerance";
    }  
    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    private void SetTolerance()
    {
        double td = Double.valueOf(jTextFieldTolerance.getText());
        tolerance.SetTolerance(td);
       // jButtonUpdate.setEnabled(false);
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
    private boolean Validate()
    {
        String tolerance = jTextFieldTolerance.getText();
        if (isNumeric(tolerance))
        {
            return true;
        }
        else
        {
            return false;
        }
    }
     private void AddChangeListener()
    {
        jTextFieldTolerance.getDocument().addDocumentListener(new DocumentListener() {
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
        
        jButtonUpdate.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent ae) {
                if (Validate())
                {
                    
                    SetTolerance();
                     AddTolerancePanel(); 
                //AddNodePanel();
        jButtonUpdate.setEnabled(false);
        //repaint();
        //this.setVisible(true);
                  // jButtonUpdate.setEnabled(false);
                }
                 //To change body of generated methods, choose Tools | Templates.
            }
        });
        
    }
    public static Tolerance GetTolerance()
    {
        return tolerance;
    }
    
    
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 800, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 560, Short.MAX_VALUE)
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables
    }
}
