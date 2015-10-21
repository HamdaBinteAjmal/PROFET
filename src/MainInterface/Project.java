/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package MainInterface;

import Nodes.Constant;
import Nodes.EquationInfo;
import Nodes.GlobalLists;
import Nodes.InputNode;
import Nodes.ModelParameterNode;
import Nodes.ModelVariableNode;
import RunInference.GeneralSetting;
import RunInference.GeneralSettingsWindow;
import RunInference.Tolerance;
import RunInference.ToleranceWindow;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;

/**
 *
 * @author Administrator
 */
public class Project {
    
    public static class ProjectInner {
    
        
    public static void saveAsFile(String filePath) {

        try
        {
            try (ObjectOutputStream expressionFile = new ObjectOutputStream(new FileOutputStream(filePath))) {
                expressionFile.writeObject(GlobalLists.EQUATIONS);
                expressionFile.writeObject(GlobalLists.ModelParameters);
                expressionFile.writeObject(GlobalLists.ModelVariables);
                expressionFile.writeObject(GlobalLists.InputNodes);
                expressionFile.writeObject(GlobalLists.Constants);
                expressionFile.writeObject(Timestep.TimestepInner.GetTimestep());
                
                expressionFile.writeObject(Tree.TreeStatic.GetGeneralSetting());
                expressionFile.writeObject(ToleranceWindow.ToleranceWindowInner.GetTolerance());
                //expressionFile.writeObject(Tree.TreeStatic.GetDBNName());
                expressionFile.close();
                // statusBar.println("Expression saved");
            }
        } catch (FileNotFoundException ex) {
               // JOptionPane.showMessageDialog(null, langMan.readLangFile("SavingExp"), "DragMath", JOptionPane.ERROR_MESSAGE);
          //  } catch (ParseException ex) {
             //   JOptionPane.showMessageDialog(null, langMan.readLangFile("SavingExp"), "DragMath", JOptionPane.ERROR_MESSAGE);
            } catch (IOException ex) {
                JOptionPane.showMessageDialog(null, ex.getMessage(), "DragMath", JOptionPane.ERROR_MESSAGE);
            }
        }
    
    public static void openFile(String path) {

        
        try
        {
                
            ObjectInputStream expressionFile = new ObjectInputStream(new FileInputStream(path));
                
            HashSet<EquationInfo> EQUATIONS = (HashSet<EquationInfo>)expressionFile.readObject();
            HashMap<ModelParameterNode, Integer> ModelParameters = (HashMap<ModelParameterNode, Integer>) expressionFile.readObject();
            HashMap<ModelVariableNode, Integer> ModelVariables = (HashMap<ModelVariableNode, Integer>) expressionFile.readObject();
            HashMap<InputNode, Integer> InputNodes = (HashMap<InputNode, Integer>) expressionFile.readObject();
            HashMap<Constant, Integer> Constants = (HashMap<Constant, Integer>) expressionFile.readObject();
            double timestep = (double) expressionFile.readObject();
            GeneralSetting st = (GeneralSetting) expressionFile.readObject();
            Tolerance tolerance = (Tolerance) expressionFile.readObject();
            //String name = (String) expressionFile.readObject();
            
            expressionFile.close();
                
            GlobalLists.LoadProject(EQUATIONS, ModelParameters, ModelVariables, InputNodes, Constants);
            Tree.TreeStatic.ReloadTimestepWindow(timestep);
            Tree.TreeStatic.ReloadSettingsWindow(st);
            Tree.TreeStatic.ReloadToleranceWindow(tolerance);
           // Tree.TreeStatic.ReloadProjectName(name);
            
            
                        
            
                
            } 
        catch (FileNotFoundException ex) {
              //  JOptionPane.showMessageDialog(null, langMan.readLangFile("LoadingExp"), "DragMath", JOptionPane.ERROR_MESSAGE);
            } catch (ClassNotFoundException ex) {
              //  JOptionPane.showMessageDialog(null, langMan.readLangFile("LoadingExp"), "DragMath", JOptionPane.ERROR_MESSAGE);
            } catch (IOException ex) {
              //  JOptionPane.showMessageDialog(null, langMan.readLangFile("LoadingExp"), "DragMath", JOptionPane.ERROR_MESSAGE);
            }
        
        
        }
    }
    
    
    }


 
