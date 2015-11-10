/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package MainInterface;
import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.TrayIcon;
import javax.swing.JToolBar;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.net.URL;
import javax.swing.Box;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JSeparator;
import javax.swing.SwingConstants;
import javax.swing.UIManager;



/**
 *
 * @author Administrator
 */
public class Toolbar extends javax.swing.JPanel implements ActionListener{
    private JToolBar toolbar;
    static final private String HOME = "GoToHome";
    static final private String NEWPROJECT = "NewProject";
    static final private String OPENPROJECT = "OpenProject";
    static final private String NEWEQUATION = "NewEquation";
    static final private String RUNINFERENCE = "BuildDBN";
    static final private String GENERATEDBN = "GenerateDBN";
    static final private String SAVEPROJECT = "SaveProject";
    static final private String PREVIOUS = "PreviousScreen";
    static final private String NEXT = "NextScreen";
    // MainForm f1;
    
    /**
     * Creates new form Toolbar
     */
    public Toolbar() {
        super(new FlowLayout());
        
        initComponents();
        toolbar = new JToolBar("");  
        AddToolbarButtons();
        toolbar.setPreferredSize(this.getSize());
        toolbar.setFloatable(false);
        add(toolbar, java.awt.BorderLayout.CENTER);
       //  f1 = (MainForm)javax.swing.SwingUtilities.getWindowAncestor(Toolbar.this);
        
        
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        setMaximumSize(new java.awt.Dimension(800, 40));
        setMinimumSize(new java.awt.Dimension(800, 40));
        setPreferredSize(new java.awt.Dimension(800, 40));
        setRequestFocusEnabled(false);
        setLayout(new java.awt.BorderLayout());
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables
void AddToolbarButtons(){
    JButton button = null;
    Icon HomeIcon = GetIcon("Home.png");          
    button = MakeToolBarButton(HomeIcon, HOME,
                                  "Open Home Screen",
                                  "HomeScreen");
    
    toolbar.add(button);
    
    Icon newProjIcon = GetIcon("NewProject.png");
    button = MakeToolBarButton(newProjIcon, NEWPROJECT,
                                  "Create a new project",
                                  "New Project");
    toolbar.add(button);
    
    Icon openProjIcon = GetIcon("OpenProject.png");
    button = MakeToolBarButton(openProjIcon, OPENPROJECT,
                                  "Open an existing project",
                                  "Open Project");
    toolbar.add(button);
    
    Icon saveIcon = GetIcon("Save.png");
    button = MakeToolBarButton(saveIcon, SAVEPROJECT,
                                  "Save Project to a File",
                                  "Save Project");
    toolbar.add(button);
    
    toolbar.addSeparator();
    Icon generateDBNIcon = GetIcon("GenerateDBN.png");
    button = MakeToolBarButton(generateDBNIcon, GENERATEDBN,
                                  "Generate DBN",
                                  "Generate DBN");
    toolbar.add(button);
       
    Icon runInferenceIcon = GetIcon("RunInference.png");
    button = MakeToolBarButton(runInferenceIcon, RUNINFERENCE,
                                  "Run Inference on DBN",
                                  "Run Inference");
    toolbar.add(button);
    
    
    
   // toolbar.add(Box.createHorizontalStrut(10));
    toolbar.addSeparator();
   // toolbar.add(new JSeparator(SwingConstants.VERTICAL));
   // toolbar.add(Box.createHorizontalStrut(10));
    
    Icon newEquationIcon = GetIcon("NewEquation.png");
    button = MakeToolBarButton(newEquationIcon, NEWEQUATION,
                                  "Open Equation Editor",
                                  "New Equation");
    toolbar.add(button);
    //toolbar.add(Box.createHorizontalStrut(10));
   // toolbar.addSeparator();
    
  //  toolbar.add(Box.createHorizontalStrut(50));
    
//   Icon backIcon = GetIcon("Back.png");
//    button = MakeToolBarButton(backIcon, PREVIOUS,
//                                  "Go to Previous Screen",
//                                  "Previous Window");
//    toolbar.add(button);
//    
//    Icon nextIcon = GetIcon("Next.png");
//    button = MakeToolBarButton(nextIcon, NEXT,
//                                  "Go to Next Screen",
//                                  "Next Window");
    toolbar.add(button);
    
    

}
ImageIcon GetHomeIcon()
{
    ImageIcon icon = new ImageIcon();
    //first button
    URL imageURL = Toolbar.class.getResource("Images/HomeFolder.gif");
    if (imageURL != null) {  
      icon = new ImageIcon(imageURL);
    }
    return icon;
}
Icon GetIcon(String imageName)
{
    ImageIcon newProIcon = new ImageIcon();
    //first button
    URL imageURL = Toolbar.class.getResource("Images/" + imageName);
    if (imageURL != null) {  
       newProIcon = new ImageIcon(imageURL);
    }
    
    return  newProIcon;
}
    JButton MakeToolBarButton(Icon icon,
                                       String actionCommand,
                                       String toolTipText,
                                       String altText){
    JButton button = new JButton();
    button.setActionCommand(actionCommand);
    button.setToolTipText(toolTipText);        //image found
    button.setIcon(icon);
    button.addActionListener(this);
    button.setFocusable(false);
    return button;
    }

public void actionPerformed(ActionEvent e) {
        String cmd = e.getActionCommand();
        String description = null;
        if (cmd.equals(NEWPROJECT))
        {
            CreateNewProject();
        }
        if (cmd.equals(OPENPROJECT))
        {
            OpenProject();
        }
        if (cmd.equals(HOME))
        {
            GoToHome();
        }
        if(cmd.equals(RUNINFERENCE))
        { 
            RunInference();
        }
        if (cmd.equals(GENERATEDBN))
        {
            GenerateDBN();
        }
        if (cmd.equals(NEWEQUATION))
        {
            AddNewEquation();
        }
        if (cmd.equals(SAVEPROJECT))
        {
            SaveProject();
        }
//        if (cmd.equals(PREVIOUS))
//        {
//            PreviousScreen();
//        }
//        if (cmd.equals(NEXT))
//        {
//            NextScreen();
//        }
        
        // Handle each button inside this function
}
private void OpenProject()
{
    MainForm.MainFormInner.LoadProject();
}
private void SaveProject()
{
    MainForm.MainFormInner.SaveProject();
}

private void CreateNewProject()
{
    MainForm.MainFormInner.OpenNewProject();
}
private void GoToHome()
{
    MainForm.MainFormInner.OpenHomeScreen();
}
private void RunInference()
{
    MainForm.MainFormInner.RunInference();
}
private void GenerateDBN()
{
    MainForm.MainFormInner.GenerateDBN();
}
private String GetDBNName()
{
    String name = JOptionPane.showInputDialog(this, "Select a name for the DBN", "Set name", JOptionPane.QUESTION_MESSAGE); 
    File file = new File(name + ".dbn");
    boolean overwrite = false;
    while (file.exists() && overwrite == false)
    {
        int dialogResult = JOptionPane.showConfirmDialog (null,
                        "This name already exists, do you want to overwrite?");
        if(dialogResult == JOptionPane.NO_OPTION)
        {
           name = JOptionPane.showInputDialog(this, "Select a name for the DBN", "Set name", JOptionPane.QUESTION_MESSAGE); 
           file = new File(name + ".dbn");
           overwrite = false;
        }
        else if (dialogResult == JOptionPane.YES_OPTION)
        {
            overwrite = true;
        }
    }
    return name;
}
private void AddNewEquation()
{
    MainForm.MainFormInner.DisplayEquationEditor();
}
private void PreviousScreen()
{
//     MainForm f1 = (MainForm)javax.swing.SwingUtilities.getWindowAncestor(Toolbar.this);
//     f1.PreviousScreen();
          
}
private void NextScreen()
{
//     MainForm f1 = (MainForm)javax.swing.SwingUtilities.getWindowAncestor(Toolbar.this);
//     f1.NextScreen();
          
}
 
}