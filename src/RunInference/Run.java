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
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Image;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.concurrent.ExecutionException;
import javax.imageio.ImageIO;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.SwingWorker;
import javax.swing.Timer;
import javax.swing.UIManager;

/**
 *
 * @author Administrator
 */
public class Run extends javax.swing.JPanel //implements ActionListener
{

    /**
     * Creates new form Run
     */
    JPanel picturePanel = new JPanel();
    JButton runButton = new JButton("Run");
    JButton cancelButton = new JButton("Cancel");
    JTextArea outputLabel = new JTextArea(100, 15);
    JPanel staticPanel = new JPanel();
    JLabel label1, label2, label3, staticlabel;
    Animator animator;
    Worker worker;
    int pic = 0;
    public Run() {
     
        initComponents();
        BoxLayout layout = new BoxLayout(this,BoxLayout.Y_AXIS);        
        this.setLayout(layout);
       // runButton.setPreferredSize(new Dimension(300,50));
        AddActionListener();
        outputLabel.setBackground(Color.lightGray);
        outputLabel.setOpaque(false);
        Font label = UIManager.getFont("Label.font");
        outputLabel.setFont(label);
        outputLabel.setEditable(false);
        LoadPictures();
        ShowStaticPicture();
        ShowStartLabel();
        PopulatePanel();
  
    }
    public void ResetLabel()
    {
//        if(!worker.isDone()|| worker.isCancelled())
//        {
//            
//        }
//        else
//        {
//            ShowStartLabel(); 
//        }
       
    }
    private void AddActionListener()
    {
        ActionListener listenerRun = new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent ae) 
            {
                    pic = 0;
                  StartProgressThread(); // Why why why not showing
                  StartInference();
            }
        };
        runButton.addActionListener(listenerRun);
        
        ActionListener listenerCancel = new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent ae) 
            {
                 int dialogResult = JOptionPane.showConfirmDialog (null,
                         "Are you sure you want to cancel the Inference process?","Warning",JOptionPane.YES_NO_OPTION);
                 if(dialogResult == JOptionPane.YES_OPTION)
                 {
                     worker.cancel(true);
                 }
  
            
            }
        };
        cancelButton.addActionListener(listenerCancel);
    }
    private void LoadPictures()
    {
         try
        {
            URL imageURL = Run.class.getResource("Images/LOGO.png");
            
            BufferedImage myPicture = ImageIO.read(Run.class.getResource("Images/LOGO.png"));
            staticlabel = new JLabel(new ImageIcon(myPicture));             
            
        }
         catch  (IOException e)
        {
            System.out.println(e.getMessage());
        }
    }
    private void ShowStaticPicture()
    {
        picturePanel.removeAll();
        picturePanel.add(staticlabel);
        repaint();
        this.setVisible(true);
    }
    private void PopulatePanel()
    {
        this.removeAll();
        this.add(Box.createRigidArea(new Dimension(0, 30)));
        this.add(picturePanel);
        this.add(Box.createRigidArea(new Dimension(0, 30)));
        JPanel ButtonsPanel = new JPanel();
        ButtonsPanel.add(runButton);
        cancelButton.setEnabled(false);
       // ButtonsPanel.add(cancelButton); too risky to add this
        this.add(ButtonsPanel);
        this.add(Box.createRigidArea(new Dimension(0, 30)));
        this.add(outputLabel);
        repaint();
        this.setVisible(true);
    }
    
    @Override
    public String toString()
    {
        return "Run";
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
        setVerifyInputWhenFocusTarget(false);

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
    
    private void StartProgressThread()
    {
        animator = new Animator();
        picturePanel.removeAll();
        picturePanel.add(animator);
        this.setVisible(true);
        
        runButton.setEnabled(false);
        cancelButton.setEnabled(true);
        ShowProgressLabel();
    }
    private void StartInference()
    {   
        worker = new Worker();
        worker.execute();
    }
    private void ShowStartLabel(){
        outputLabel.setText("Press the button to start inference");
        outputLabel.setForeground(Color.BLACK);
        repaint();
        this.setVisible(true);        
    }
    private void ShowProgressLabel()
    {
        outputLabel.setText("Inference in progress, please wait.\nThis"
                + " may take a few minutes");
        outputLabel.setForeground(Color.decode("#006400"));
        //repaint();
        this.setVisible(true);
    }
    private void ShowFinishSuccessLabel(String filePath)
    {
         String infoMessage = "Inference completed successfully.\n"
                    + "Output is saved in \n" +
                filePath ;
         outputLabel.setText(infoMessage);
         outputLabel.setForeground(Color.BLACK);
         this.setVisible(true);
    }
    private void ShowFinishFailureLabel(String exceptionMessage)
    {
        String infoMessage = "Inference Failed due to Error \n \"";
        infoMessage = infoMessage + exceptionMessage + "\"";
        outputLabel.setText(infoMessage);
        outputLabel.setForeground(Color.RED);
        this.setVisible(true);
    }
    private void StopProgressThread(String filePath)
    {
        
        ShowStaticPicture();
        File file = new File(filePath);
        if (file.exists()) //successs
        {
            ShowFinishSuccessLabel(filePath);
           
        }    
        else
        {
            ShowFinishFailureLabel(filePath);
        }
    }
    
   
    public class Animator extends JPanel {
        JLabel gifLabel = new JLabel();
        Image img;
        ImageIcon ic;
        public Animator() {
            super();                 
            LoadGif();
        }
        public void LoadGif()
        {
           removeAll();
           img = getToolkit().createImage(getClass().getResource("Images/animation.gif"));
           ImageIcon ic = new ImageIcon(img);
           JLabel lab = new JLabel(ic);
           add(lab);
           setVisible(true);          
            
        }    
    }
       

        
        class Worker extends SwingWorker<String, Void>
        {
        @Override
        public String doInBackground() {
            Tree.TreeStatic.BlockViewDBN(true);
            String outputFilePath =  Tree.TreeStatic.RunInference();
            return outputFilePath;
        }
        @Override
        public void done() {
            String str = "";
            try {
                Tree.TreeStatic.BlockViewDBN(false);
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
                StopProgressThread(str);
                runButton.setEnabled(true);
                cancelButton.setEnabled(false);
            }
           
            
        }
        
    };
}