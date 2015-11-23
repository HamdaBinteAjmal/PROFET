/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package MainInterface;

import Graphviz.GraphVizConnector;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Desktop;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.SwingWorker;

/**
 *
 * @author Administrator
 */
public class ViewDBN  {
public static class ViewDBNInner  extends javax.swing.JPanel{

    /**
     * Creates new form ViewDBN
     */
   
        JPanel topPanel = new JPanel();
        JLabel label = new JLabel ("Structure of the Dynamic Bayesian Network");
        JLabel progressLabel = new JLabel("Generating DBN structure, this may take a few minutes.");
        private String DBNName = "";
        GraphVizConnector gv = new GraphVizConnector();
        JButton viewBtn = new JButton("Open Image");
        Worker worker;
        JPanel centerPanel = new JPanel();
        public ViewDBNInner() 
        {
            initComponents();
            BorderLayout layout = new BorderLayout();
            this.setLayout(layout);
           // this.add(label, BorderLayout.PAGE_START);
            AddActionListener();
            PopulatePanel();
            //this.DBNName = DBNName;
        
        }
        private void AddActionListener()
        {
            viewBtn.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent ae) 
                {
                
                    try {
                        Desktop.getDesktop().open(new File(GetImageFullPath()));
                    } catch (IOException ex) {
                        Logger.getLogger(ViewDBN.class.getName()).log(Level.SEVERE, null, ex);
                    }
                
                }
            });
        }       
        
        private void PopulatePanel()
        {
            
            this.removeAll();
             topPanel.removeAll();
            topPanel.add(label);
            this.add(topPanel, BorderLayout.PAGE_START);
            this.add(centerPanel, BorderLayout.CENTER);
           
            JPanel buttonPanel = new JPanel();
            buttonPanel.add(viewBtn);
            this.add(buttonPanel, BorderLayout.PAGE_END);
            viewBtn.setEnabled(false);
            this.setVisible(true);
            progressLabel.setForeground(Color.decode("#006400"));
            
        }
        private void ShowProgressLabel()
        {
            centerPanel.removeAll();
            progressLabel.setForeground(Color.decode("#006400"));
            centerPanel.add(progressLabel, BorderLayout.CENTER);
            this.revalidate();            
            this.setVisible(true);
        }
        private void StartMakeDBN()
        {
             worker = new Worker();
             worker.execute();
        }
        public void StartProcess(String dbnName)
        {
            this.DBNName = dbnName;
            PopulatePanel();
            ShowProgressLabel();
            StartMakeDBN();
        }
        private void ShowResults(String dotFilePath)
        {
            
            File f = new File(dotFilePath + ".dot");
            if(f.exists()) 
            {
                boolean success = ConvertDotToPNG();
                if (!success)
                {
                    boolean reset = ResetDOTInstallationPath();
                    if (reset)
                    {
                        ShowResults(dotFilePath);
                    }
                }
                else
                {
                    DisplayImage();
                    viewBtn.setEnabled(true);
                }
            }
            else
            {
                viewBtn.setEnabled(false);
                DisplayError(dotFilePath);
                
            }
            
            centerPanel.repaint();
            centerPanel.setVisible(true);
            this.revalidate();
            this.repaint();
            this.setVisible(true);
        }
        private void DisplayError(String error)
        {
            centerPanel.removeAll();
            
            JTextArea errorLabel = new JTextArea(100, 15);
            errorLabel.setOpaque(false); //removes white backgr
            errorLabel.setText(error);
            errorLabel.setEditable(false);
            
            errorLabel.setForeground(Color.red);
            centerPanel.add(errorLabel);
            centerPanel.repaint();
            this.setVisible(true);
        }
     
    
        private String GetImageFullPath()
        {
            String path = System.getProperty("user.dir");       // 
            path = path.replace("\\", "/");
            path = path + "/Results/";
            String type = "png";
            String imagePath = path + "DBNImages/" + DBNName + "." + type;
            return imagePath;
        }
        private void DisplayImage() 
        {
            centerPanel.removeAll();
            String imagePath = GetImageFullPath();
            try
            {
                BufferedImage myPicture = ImageIO.read(new File(imagePath));
                Image scaled;
                if (myPicture.getWidth() > 550)
                {
                    scaled = myPicture.getScaledInstance(550, -1 , Image.SCALE_SMOOTH);
                }
                else
                {
                    scaled = myPicture;
                }
                JLabel picLabel = new JLabel(new ImageIcon(scaled));
                
                centerPanel.add(picLabel);
                
                
            }
            catch (IOException e)
            {
                JOptionPane.showMessageDialog(this, "Cannot Display image!");
            }          
            centerPanel.repaint();
            this.setVisible(true);
        }
   
        private  boolean ConvertDotToPNG()
        {
            String path = System.getProperty("user.dir");       // 
            path = path.replace("\\", "/");
            path = path + "/Results/";
            String dotPath = path + "DOT/" + DBNName + ".dot";
            System.out.println(dotPath);
            String type = "png";
            String imagePath = path + "DBNImages/" + DBNName + "." + type;
       // 
            System.out.println(imagePath);
        
            File out = new File(imagePath);
            gv.readSource(dotPath);
            try
            {
                gv.writeGraphToFile( gv.getGraph( gv.getDotSource(), type ), out );
            }
            catch (Exception e) 
            {
                return false;
            }
            return true;     
       
    }
    private boolean Browse()
    {
        
        JFileChooser chooser = new JFileChooser();
        chooser.setCurrentDirectory(new java.io.File("."));
        chooser.setDialogTitle("Locate the executable in bin folder of GraphViz Installation directory");
        //chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        chooser.setAcceptAllFileFilterUsed(false);

        if (chooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) 
        {
            GraphVizConnector.ResetDOTInstallationPath( chooser.getSelectedFile().getAbsolutePath());
           // System.out.println("getCurrentDirectory(): " + chooser.getCurrentDirectory());
            //System.out.println("getSelectedFile() : " + chooser.getSelectedFile());
            return true;
        } 
        else 
        {
            System.out.println("No Selection ");
            return false;
        }  
    }
    private boolean ResetDOTInstallationPath()
    {
        boolean retVal = false;
        Object[] options = {"Browse",
                    "Later"};
        int n = JOptionPane.showOptionDialog(this,
            "Graphviz needs to be installed on your system to view the DBN as an image.\n"
                + "If it is already installed, please locate the executable (dot) in the "
                    + "installation folder.\n"
                + "If it is not installed, please install it.",
            "GraphViz",
        JOptionPane.YES_NO_OPTION,
        JOptionPane.QUESTION_MESSAGE,
        null,     //do not use a custom Icon
        options,  //the titles of buttons
        options[0]); //default button title
        
        if (n == JOptionPane.YES_OPTION)
        {
            retVal = Browse();
        }
        else
        {
            retVal = false;// do nothing here 
        }
        return retVal;
   }
        @Override
    public String toString()
    {
        return "View DBN Structure";
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
        setPreferredSize(new java.awt.Dimension(600, 500));

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, Short.MAX_VALUE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 560, Short.MAX_VALUE)
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables
 class Worker extends SwingWorker<String, Void>
        {
        @Override
        public String doInBackground() {
           // Tree.TreeStatic.BlockViewDBN(true);
            Tree.TreeStatic.BlockRunWindow(true);
            String outputFilePath =  Tree.TreeStatic.GenerateDBN();
            return outputFilePath;
        }
        @Override
        public void done() {
            String str = "";
            try {
                Tree.TreeStatic.BlockRunWindow(false);
               // Tree.TreeStatic.BlockViewDBN(false);
               // timer.stop();
                if(isCancelled())
                {
                    str = "Operation cancelled by user";
                }
                else
                {
                    str = get();
                }
            } catch (InterruptedException | ExecutionException ex) {
                 
                str = ex.getMessage();
            }
            finally
            {
                ShowResults(str);
            }
           
            
        }
        
    };    

}
}