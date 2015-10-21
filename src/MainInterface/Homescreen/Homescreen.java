/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package MainInterface.Homescreen;

import MainInterface.MainForm;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Desktop;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

/**
 *
 * @author Administrator
 */
public class Homescreen extends javax.swing.JPanel{

    /**
     * Creates new form Homescreen
     */
    JEditorPane editorPane;
    JPanel ButtonPane;
    JButton AddModelButton;
    public Homescreen() 
    {
        initComponents();
       // jScrollPane1.setBorder(null);
      //  jScrollPane2.setBorder(null); 
        this.setBackground(Color.decode("#FFFFE0"));
        this.setOpaque(true);
        this.setLayout(new BorderLayout());
        editorPane = new JEditorPane();
        ButtonPane = new JPanel();
        AddHTMLPage();
        AddButton();
        AddActionListener();
        revalidate();
        repaint();

    }
    private void AddButton()
    {
        
        AddModelButton = new JButton("Begin: Enter Mathematical Model");
        ButtonPane.add(AddModelButton);
        this.add(ButtonPane, BorderLayout.PAGE_END);
        
        ButtonPane.setOpaque(true);
        ButtonPane.setBackground(Color.decode("#FFFFE0"));
        ButtonPane.revalidate();
        ButtonPane.repaint();
        
    }
    private void AddHTMLPage()
    {
        editorPane.setEditorKit(JEditorPane.createEditorKitForContentType("text/html"));
        editorPane.setEditable(false);
        editorPane.setBorder(BorderFactory.createEmptyBorder(0,0,0,0));
        java.net.URL helpURL = Homescreen.class.getResource(
                                "Homescreen.html");
        
        if (helpURL != null) 
        {
            try 
            {
                editorPane.setPage(helpURL);
                this.add(editorPane, BorderLayout.CENTER);
            }
            catch (IOException e) 
            {
                System.err.println("Attempted to read a bad URL: " + helpURL);
            }
        } 
        else 
        {
            System.err.println("Couldn't find file: Homescreen.html");
        }
        
    }
    private void AddActionListener()
    {
        editorPane.addHyperlinkListener(new HyperlinkListener() 
        {
            @Override
            public void hyperlinkUpdate(HyperlinkEvent e) 
            {
               if(e.getEventType() == HyperlinkEvent.EventType.ACTIVATED) 
               {
                // Do something with e.getURL() here
                    if(Desktop.isDesktopSupported()) {
                    try 
                    {
                        Desktop.getDesktop().browse(e.getURL().toURI());
                    } 
                    catch (IOException | URISyntaxException ex) 
                    {
                        Logger.getLogger(Homescreen.class.getName()).log(Level.SEVERE, null, ex);
                    }
                    }
                }
            }
        });
        
            
        AddModelButton.addActionListener(new ActionListener() 
        {

            @Override
            public void actionPerformed(ActionEvent ae) { 
             
                MainForm.MainFormInner.DisplayEquationEditor();
                }
                
           });
    }
    
    

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel1 = new javax.swing.JPanel();

        setMinimumSize(new java.awt.Dimension(600, 525));
        setName(""); // NOI18N
        setPreferredSize(new java.awt.Dimension(600, 525));

        javax.swing.GroupLayout jPanel1Layout = new javax.swing.GroupLayout(jPanel1);
        jPanel1.setLayout(jPanel1Layout);
        jPanel1Layout.setHorizontalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 284, Short.MAX_VALUE)
        );
        jPanel1Layout.setVerticalGroup(
            jPanel1Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 217, Short.MAX_VALUE)
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(145, 145, 145)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(171, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGap(135, 135, 135)
                .addComponent(jPanel1, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap(198, Short.MAX_VALUE))
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JPanel jPanel1;
    // End of variables declaration//GEN-END:variables
}
