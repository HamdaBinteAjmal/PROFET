/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package MainInterface;

import Lisp.GenerateDBN;
import Lisp.RunInferenceAdaptiveTimestep;
import Lisp.RunInferenceFixedTimestep;
import Nodes.GlobalLists;
import Nodes.Constant;
import Nodes.EquationInfo;
import Nodes.Frames.ConfusedNodes;
import Nodes.Frames.IntendedPanel;
import Nodes.Frames.ModelParameterNodeWindow;
import Nodes.Frames.ModelVariableNodeWindow;
import Nodes.Frames.ObservedPanel;
import Nodes.InputNode;
import Nodes.ModelParameterNode;
import Nodes.ModelVariableNode;
import RunInference.GeneralSetting;
import RunInference.GeneralSettingsWindow;
import RunInference.Run;
import RunInference.SetConstants;
import RunInference.Tolerance;
import java.awt.GridLayout;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Map;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
/**
 *
 * @author Administrator
 */
 

public class Tree extends JPanel{
    
   
    public static class TreeStatic extends JPanel
    {
    protected static DefaultMutableTreeNode rootNode;
    protected static DefaultMutableTreeNode generateDBNNode;
    protected static DefaultMutableTreeNode viewDBNNode;
    protected static DefaultMutableTreeNode runInferenceNode;
    protected static DefaultMutableTreeNode ODENodes;
    protected static DefaultMutableTreeNode DBNNodes;
    protected static DefaultMutableTreeNode modelParameterNodes;
    protected  static  DefaultMutableTreeNode modelVariableNodes;
    protected static DefaultMutableTreeNode inputNodes;
    protected static DefaultMutableTreeNode ResultsNodes;
    
    protected static DefaultTreeModel treeModel;
    protected static JTree tree;
    private static Toolkit toolkit = Toolkit.getDefaultToolkit();
    
    private static final String ODEName = "Ordinary Differential Equations";
    //private final String ConstantsName = "Choose Constants";
    public static MainInterface.Timestep.TimestepInner timestep ;
   //  private static RunInference.SetConstants SetConstantsWindow;
    private static ConfusedNodes.ConfusedNodesInner confusedNodes;
    //private final String TimestepName = "Timestep";
    private static final String NodesName = "Nodes";
    private static RunInference.GeneralSettingsWindow.GeneralSettingsInner GeneralSettingsWindow;
    private static RunInference.EvidenceUploadWindow.EvidenceUploadWindowInner evidenceWindow;
    private static RunInference.OutputNodes.OutputNodesInner outputNodesWindow;
         //private MainForm mainForm;

    private static String DBNName = "Untitled";
    private static GenerateDBN generateDBN = null;
    private static Lisp.RunInferenceFixedTimestep fixedTimestepInference = null;
    private static Lisp.RunInferenceAdaptiveTimestep adaptiveTimestepInference = null;
    private static ViewDBN.ViewDBNInner viewDBN= null;
    private static RunInference.Graphs.Graphs.GraphsInner graph = null;
    private static RunInference.ToleranceWindow.ToleranceWindowInner toleranceWindow = null;
    private static RunInference.Run runWindow = null;
   // private 
    private static String ProjectPath = "";
    private static boolean BlockViewDBN = false;
    private static boolean BlockRunWindow = false;
    //private static Vector<JPanel> windows = null;  
    

     
     public TreeStatic() {
   // public void initialize()
    
        //this.setLayout(new GridLayout(1,0));
        super(new GridLayout(1,0));
        

        rootNode = new DefaultMutableTreeNode(DBNName);
        
        treeModel = new DefaultTreeModel(rootNode);
        timestep = new Timestep.TimestepInner();
        GeneralSettingsWindow = new GeneralSettingsWindow.GeneralSettingsInner();
        viewDBN = new ViewDBN.ViewDBNInner();
        graph = new RunInference.Graphs.Graphs.GraphsInner();
        
        confusedNodes = new ConfusedNodes.ConfusedNodesInner();
        tree = new JTree(treeModel);
        tree.setEditable(false);
        tree.getSelectionModel().setSelectionMode
                (TreeSelectionModel.SINGLE_TREE_SELECTION);
        tree.setShowsRootHandles(true);
        JScrollPane scrollPane = new JScrollPane(tree);
        add(scrollPane);
        
        
        createNodes(rootNode);
        evidenceWindow = new RunInference.EvidenceUploadWindow.EvidenceUploadWindowInner();
        outputNodesWindow = new RunInference.OutputNodes.OutputNodesInner();
        toleranceWindow = new RunInference.ToleranceWindow.ToleranceWindowInner();
        runWindow  = new Run();
        Tree.TreeStatic.OpenInferenceNodes();
        Tree.TreeStatic.OpenResultsNodes();
        AddMouseListener();
        
        
        
        
    }
     
  
    public static ArrayList<Constant> GetAllConstants()
    {
        ArrayList<Constant> constants = new ArrayList<>();
        Map<Constant, Integer> map = GlobalLists.Constants;
        for (Constant key : map.keySet()) 
        {
            constants.add(key);    // ...
        }
        return constants;
        
    }
    public static ArrayList<EquationInfo> GetAllEquations()
    {
        ArrayList<EquationInfo> equations = new ArrayList<>();
        DefaultMutableTreeNode eqNodes = searchNode(ODEName, generateDBNNode);
        int count = eqNodes.getChildCount();
        for (int i = 0 ; i < count; i++)
        {
            DefaultMutableTreeNode n = (DefaultMutableTreeNode)eqNodes.getChildAt(i); 
            equations.add((EquationInfo)n.getUserObject());
        }
        return equations;
        
    }
    public static ArrayList<ModelParameterNode> GetAllModelParameterNodes()
    {
        ArrayList<ModelParameterNode> nodes = new ArrayList<>();
        DefaultMutableTreeNode mpNodes = searchNode("Model Parameters", DBNNodes);
       
        if (mpNodes != null)
        {
             int count = mpNodes.getChildCount();
            for (int i = 0 ; i < count; i++)
            {
                DefaultMutableTreeNode n = (DefaultMutableTreeNode)mpNodes.getChildAt(i); 
                nodes.add((ModelParameterNode)n.getUserObject());
            }
        }
        return nodes;
    }
    public static ArrayList<ModelVariableNode> GetAllModelVariableNodes()
    {
        ArrayList<ModelVariableNode> nodes = new ArrayList<>();
        DefaultMutableTreeNode mvNodes = searchNode("Model Variables", DBNNodes);
        
        if (mvNodes != null)
        {
            int count = mvNodes.getChildCount();
            for (int i = 0 ; i < count; i++)
            {
                DefaultMutableTreeNode n = (DefaultMutableTreeNode)mvNodes.getChildAt(i); 
                nodes.add((ModelVariableNode)n.getUserObject());
            }
        }
        return nodes;
    }
    public static ArrayList<InputNode> GetAllInputNodes()
    {
        ArrayList<InputNode> nodes = new ArrayList<>();
        DefaultMutableTreeNode iNodes = searchNode("Evidence Nodes", DBNNodes);
        if (iNodes != null)
        {
            int count = iNodes.getChildCount();
            for (int i = 0 ; i < count; i++)
            {
                DefaultMutableTreeNode n = (DefaultMutableTreeNode)iNodes.getChildAt(i);            
                nodes.add((InputNode)n.getUserObject());
            }
             
        }
        return nodes;
        
    }
    private static void createNodes(DefaultMutableTreeNode top)
    {
     
        
        DefaultMutableTreeNode node = null;
        
        generateDBNNode = new DefaultMutableTreeNode("Generate DBN Structure");
        top.add(generateDBNNode);
        
        ODENodes = new DefaultMutableTreeNode(ODEName);
        generateDBNNode.add(ODENodes);
       
        node = new DefaultMutableTreeNode(confusedNodes);
        generateDBNNode.add(node);
        node = new DefaultMutableTreeNode(timestep);
        generateDBNNode.add(node);
        
        DBNNodes = new DefaultMutableTreeNode(NodesName);
        generateDBNNode.add(DBNNodes);
        
        inputNodes = new DefaultMutableTreeNode("Evidence Nodes");
        DBNNodes.add(inputNodes);
        modelParameterNodes = new DefaultMutableTreeNode("Model Parameters");
        DBNNodes.add(modelParameterNodes);
        modelVariableNodes = new DefaultMutableTreeNode("Model Variables");
        DBNNodes.add(modelVariableNodes);
        
       
        viewDBNNode = new DefaultMutableTreeNode(viewDBN);
        top.add(viewDBNNode);
        
        runInferenceNode = new DefaultMutableTreeNode("Run Inference");
        ResultsNodes = new DefaultMutableTreeNode("Results");
        top.add(runInferenceNode);       
        top.add(ResultsNodes);
          
        
        
    }
     public static void RemoveNodeFromTree(Object node)
    {
        String nodeType = node.getClass().getName();
        switch (nodeType) {
            case "Nodes.InputNode":
                {
                DefaultMutableTreeNode InputNodes;
                InputNodes = searchNode("Evidence Nodes", DBNNodes);
                DefaultMutableTreeNode sameNameNode = searchNode(node.toString(), InputNodes);
                    if (sameNameNode != null)
                    {
                    InputNodes.remove(sameNameNode);
                    treeModel.reload(InputNodes);
                    }
                }
             case "Nodes.ModelParameterNode":
                {
                    DefaultMutableTreeNode ModelParameterNodes = null;
                    ModelParameterNodes = searchNode("Model Parameters", DBNNodes);
                    DefaultMutableTreeNode sameNameNode = searchNode(node.toString(), ModelParameterNodes);
                    if (sameNameNode != null)
                    {
                        ModelParameterNodes.remove(sameNameNode);
                        treeModel.reload(ModelParameterNodes);
                    }
                
                }
             case "Nodes.ModelVariableNode":
             {
               
                DefaultMutableTreeNode ModelVariableNodes = null;
                ModelVariableNodes = searchNode("Model Variables", DBNNodes);
                DefaultMutableTreeNode sameNameNode = searchNode(node.toString(), ModelVariableNodes);
                if (sameNameNode != null)
                {
                    ModelVariableNodes.remove(sameNameNode);
                    treeModel.reload(ModelVariableNodes);
                }   
             }
        }
    }
     public static void LoadEquationFromFile(EquationInfo eq) {
             DefaultMutableTreeNode ODENode = null;
        ODENode = searchNode(ODEName, ODENodes);
        ODENode.add(new DefaultMutableTreeNode(eq));
     }
     
     
    public static void AddNodeToTree(Object node)
    {
        String nodeType = node.getClass().getName();
        
        DefaultTreeModel model = (DefaultTreeModel)tree.getModel();
        //DefaultMutableTreeNode root = (DefaultMutableTreeNode)model.getRoot();
        switch (nodeType) {
            
            case "Nodes.InputNode":
                {
                    DefaultMutableTreeNode InputNodes;// = null;
                    InputNodes = searchNode("Evidence Nodes", DBNNodes);
                    DefaultMutableTreeNode sameNameNode = searchNode(node.toString(), InputNodes);
                    if( sameNameNode == null)
                    {
                        DefaultMutableTreeNode newNode = new DefaultMutableTreeNode((InputNode)node);
                        InputNodes.add(newNode);
                        model.reload(InputNodes);
                    }
                    else
                    {
                        
                    }       break;
                }
            case "Nodes.ModelParameterNode":
            {
                DefaultMutableTreeNode ModelParameterNodes = null;
                ModelParameterNodes = searchNode("Model Parameters", DBNNodes);
                DefaultMutableTreeNode sameNameNode = searchNode(node.toString(), ModelParameterNodes);
                if (sameNameNode == null)
                {
                    ModelParameterNodes.add(new DefaultMutableTreeNode(node));
                    model.reload(ModelParameterNodes);
                }
                else
                {
                
            }       break;
                }
            case "Nodes.ModelVariableNode":
            {
                DefaultMutableTreeNode ModelVariableNodes = null;
                ModelVariableNodes = searchNode("Model Variables", DBNNodes);
                DefaultMutableTreeNode sameNameNode = searchNode(node.toString(), ModelVariableNodes);
                if (sameNameNode == null)
                {
                    ModelVariableNodes.add(new DefaultMutableTreeNode(node));
                    model.reload(ModelVariableNodes);
                }
                else
                {
                
            }       break;
                }
        }
        tree.setVisible(true);
    }
    public static void AddEquation(EquationInfo eq)//MathObject obj, String eqStr, InputComponent[] inputComponents){
    {    
        DefaultTreeModel model = (DefaultTreeModel)tree.getModel();
        DefaultMutableTreeNode root = (DefaultMutableTreeNode)model.getRoot();
        //root.add(new DefaultMutableTreeNode("another_child"));
        DefaultMutableTreeNode ODENode = null;
        ODENode = searchNode(ODEName, ODENodes);
        try 
        {
            EquationInfo newEq = eq;//new EquationInfo(obj, eqStr, inputComponents); 
            DefaultMutableTreeNode sameNameNode = searchNode(newEq.toString(), ODENode);
            
            if( sameNameNode != null)
            {
                int dialogResult = JOptionPane.showConfirmDialog (null,
                        "Equation " + newEq.toString() + " already exists, do you want to overwrite?"
                                + "\nWarning: This will reset all values"
                               );
                if(dialogResult == JOptionPane.YES_OPTION)
                {
                    EquationInfo oldEq = (EquationInfo)sameNameNode.getUserObject();
                    
                   ODENode.remove(sameNameNode); 
                    GlobalLists.DeleteEquation(oldEq);
                    ODENode.add(new DefaultMutableTreeNode(newEq));
                    GlobalLists.AddEquation((EquationInfo)newEq);
                }
                else
                {
                    //Do Nothing if no or cancel
                }
            }
            else
            {
                ODENode.add(new DefaultMutableTreeNode(newEq));
                GlobalLists.AddEquation((EquationInfo)newEq);
            }
            
            
        }
        catch (IllegalArgumentException ex)
        {
            JOptionPane.showMessageDialog(null, ex.getMessage(), "Invalid Equation", JOptionPane.ERROR_MESSAGE);
        }
        model.reload(root);
    }
    public static DefaultMutableTreeNode searchNode(String nodeStr, DefaultMutableTreeNode startNode) {
        DefaultMutableTreeNode node = null;
        
        Enumeration e = startNode.breadthFirstEnumeration();
        while (e.hasMoreElements()) {
            node = (DefaultMutableTreeNode) e.nextElement();
                if (nodeStr.equals(node.getUserObject().toString())) {
                    return node;
                }
            }
    return null;
  }
    /** Remove all nodes except the root node. */
    public static void clear() {
        rootNode.removeAllChildren();
        treeModel.reload();
    }

    /** Remove the currently selected node. */
    public void removeCurrentNode() {
        TreePath currentSelection = tree.getSelectionPath();
        if (currentSelection != null) {
            DefaultMutableTreeNode currentNode = (DefaultMutableTreeNode)
                         (currentSelection.getLastPathComponent());
            MutableTreeNode parent = (MutableTreeNode)(currentNode.getParent());
            if (parent != null) {
                treeModel.removeNodeFromParent(currentNode);
                return;
            }
        } 

        // Either there was no selection, or the root was selected.
        toolkit.beep();
    }

    /** Add child to the currently selected node. */
    public DefaultMutableTreeNode addObject(Object child) {
        DefaultMutableTreeNode parentNode = null;
        TreePath parentPath = tree.getSelectionPath();

        if (parentPath == null) {
            parentNode = rootNode;
        } else {
            parentNode = (DefaultMutableTreeNode)
                         (parentPath.getLastPathComponent());
        }

        return addObject(parentNode, child, true);
    }

    public DefaultMutableTreeNode addObject(DefaultMutableTreeNode parent,
                                            Object child) {
        return addObject(parent, child, false);
    }

    public DefaultMutableTreeNode addObject(DefaultMutableTreeNode parent,
                                            Object child, 
                                            boolean shouldBeVisible) {
        DefaultMutableTreeNode childNode = 
                new DefaultMutableTreeNode(child);

        if (parent == null) {
            parent = rootNode;
        }
	
	//It is key to invoke this on the TreeModel, and NOT DefaultMutableTreeNode
        treeModel.insertNodeInto(childNode, parent, 
                                 parent.getChildCount());

        //Make sure the user can see the lovely new node.
        if (shouldBeVisible) {
            tree.scrollPathToVisible(new TreePath(childNode.getPath()));
        }
        return childNode;
    }
    
    private void RightClick(MouseEvent e){
        TreePath path = tree.getPathForLocation ( e.getX (), e.getY () );
                Rectangle pathBounds = tree.getUI ().getPathBounds ( tree, path );
                if ( pathBounds != null && pathBounds.contains ( e.getX (), e.getY () ) )
                {
                    DefaultMutableTreeNode node = (DefaultMutableTreeNode)tree.getLastSelectedPathComponent();
                    if (node == null) return;
                    Object nodeInfo = node.getUserObject();
                    String s = nodeInfo.getClass().getName();
                                       
                    if (s.equals("Nodes.EquationInfo")){
                        
                        ShowEquationPopUpMenu(pathBounds, (EquationInfo)nodeInfo);
                    }
                    
                }
    }
    private void ShowEquationPopUpMenu(Rectangle pathBounds, EquationInfo eqObj){
        AddEquationNodePopUpMenu menu = new AddEquationNodePopUpMenu(TreeStatic.this, eqObj);
        menu.show ( tree, pathBounds.x, pathBounds.y + pathBounds.height );
        
    }
    private void DoubleClick(MouseEvent e) {
        
        DefaultMutableTreeNode node = (DefaultMutableTreeNode)tree.getLastSelectedPathComponent();
        if (node == null) 
            return;
        Object nodeInfo = node.getUserObject();
        String s = nodeInfo.getClass().getName();
        
        if (s.equals("Nodes.EquationInfo")){
            EditExistingEquation((EquationInfo)nodeInfo);
        }
        else if(s.equals("MainInterface.Timestep$TimestepInner"))
            {
                ChangeView((JPanel)timestep);
            }
        else if(s.equals("Nodes.Frames.ConfusedNodes$ConfusedNodesInner"))
        {
            //confusedNodes.UpdateList();
            ConfusedNodes.ConfusedNodesInner panel= new ConfusedNodes.ConfusedNodesInner();
            ChangeView((JPanel)panel);
        }
        else if(s.equals("Nodes.InputNode"))
        {
            InputNode iNode = (InputNode)nodeInfo;
            if(iNode.IsObserveableNode())
            {
                ObservedPanel panel = new ObservedPanel(iNode);
                ChangeView(panel);
            }
            else
            {
                IntendedPanel panel = new IntendedPanel(iNode);
                ChangeView(panel);
            }
        }
        else if(s.equals("Nodes.ModelParameterNode"))
        {
            ModelParameterNodeWindow panel = new ModelParameterNodeWindow((ModelParameterNode)nodeInfo);
            ChangeView(panel);
        }
          else if(s.equals("Nodes.ModelVariableNode"))
        {
            ModelVariableNodeWindow panel = new ModelVariableNodeWindow((ModelVariableNode)nodeInfo);
            ChangeView(panel);
        }
          else if (s.equals("RunInference.SetConstants$SetConstantsInner"))
          {
              SetConstants.SetConstantsInner panel = new SetConstants.SetConstantsInner();
             // SetConstantsWindow.Update();
              ChangeView(panel);
          }
        else if (s.equals("RunInference.GeneralSettingsWindow$GeneralSettingsInner"))
          {
              //SetConstantsWindow.Update();
              GeneralSettingsWindow.SetNaturalTimestepLabel(Timestep.TimestepInner.GetTimestep());
              ChangeView(GeneralSettingsWindow);
          }
        else if (s.equals("RunInference.EvidenceUploadWindow$EvidenceUploadWindowInner"))
          {
              evidenceWindow = new RunInference.EvidenceUploadWindow.EvidenceUploadWindowInner();
              evidenceWindow.Update();
              //SetConstantsWindow.Update();
              ChangeView(evidenceWindow);
          }
        else if (s.equals("RunInference.OutputNodes$OutputNodesInner"))
        {
            outputNodesWindow = new RunInference.OutputNodes.OutputNodesInner();
            outputNodesWindow.Update();
            ChangeView(outputNodesWindow);
        }
        else if (s.equals("MainInterface.ViewDBN$ViewDBNInner"))
        {
            OpenViewDBNWindow();
//            GenerateDBN();
//            viewDBN.Update(DBNName);
//            ChangeView(viewDBN);
        }
        else if (s.equals("RunInference.Graphs.Graphs$GraphsInner"))
        {
            ChangeView(graph);
        }
        else if (s.equals("RunInference.ToleranceWindow$ToleranceWindowInner"))
        {
            boolean  enable = !GeneralSettingsWindow.GetSetting().isFixedTimestep();
            
            if (!enable)
            {
                JOptionPane.showMessageDialog(this, "Can not set tolerance for standard fixed time step Particle Filtering", "Tolerance Setting", JOptionPane.INFORMATION_MESSAGE);
   
            }
            else
            {
                toleranceWindow.Update();
                ChangeView(toleranceWindow);
            }
        }
        else if(s.equals("RunInference.Run"))
        {
            OpenRunInferenceWindow();
            
        }
        
        
    }
    public static void BlockViewDBN(boolean block)
    {
        BlockViewDBN = block;        
    }
    public static void BlockRunWindow(boolean block)
    {
        BlockRunWindow = block;
    }
    public static  void OpenViewDBNWindow()
    {
        if (!BlockViewDBN)
        {   
            ChangeView(viewDBN);
//            if (CheckMathematicalModel())
//            {
                //Tree.TreeStatic.GenerateDBN();
                viewDBN.StartProcess(DBNName);
         //   }
//            else
//            {
//                viewDBN.UpdateWithError();
//            }
            
        }
        else
        {
             JOptionPane.showMessageDialog(null, "Inference is running, please wait.",
                     "Inference Running", JOptionPane.INFORMATION_MESSAGE);
   
        }
    }
    private static void ChangeView(JPanel panel)
    {
        MainForm.MainFormInner.ChangeView(panel);
//         MainForm f = (MainForm)javax.swing.SwingUtilities.getWindowAncestor(this);
//        f.ChangeView(panel);
    }
    public void AddNewEquation()
    {
        MainForm.MainFormInner.DisplayEquationEditor();
//        MainForm f = (MainForm)javax.swing.SwingUtilities.getWindowAncestor(this);
//        f.DisplayEquationEditor();
//        f.repaint();
     
     
    }
    public void EditExistingEquation(EquationInfo eqObj)
    {
        MainForm.MainFormInner.DisplayEquationEditor(eqObj.GetExpressionTree());
//        MainForm f = (MainForm)javax.swing.SwingUtilities.getWindowAncestor(this);
//        f.DisplayEquationEditor(eqObj.GetExpressionTree());
//        f.repaint();
    }
    public void DeleteExistingEquation(EquationInfo eqObj)
    {
        removeCurrentNode();
        GlobalLists.DeleteEquation(eqObj);
        AddNewEquation();
    }
    public static void SetEvidenceForInputNode(InputNode node)
    {
        DefaultMutableTreeNode inputNodes;
        inputNodes = searchNode("Evidence Nodes", DBNNodes);
        DefaultMutableTreeNode inputNode ;
        inputNode = searchNode(node.toString(), inputNodes);
        if (inputNode != null)
        {
            inputNode.setUserObject(node);
        }
        
    }
    private static void OpenResultsNodes()
    {
        DefaultMutableTreeNode node = new DefaultMutableTreeNode(graph);
        ResultsNodes.add(node);
    }
    public static void OpenInferenceNodes()
    {
        DefaultMutableTreeNode InferenceNodes;
        InferenceNodes = searchNode("Run Inference", rootNode);
        SetConstants.SetConstantsInner panel = new SetConstants.SetConstantsInner();
        DefaultMutableTreeNode node = new DefaultMutableTreeNode(panel);
        InferenceNodes.add(node);
        
        node = new DefaultMutableTreeNode(GeneralSettingsWindow);
        InferenceNodes.add(node);
        
        
        node = new DefaultMutableTreeNode(evidenceWindow);
        InferenceNodes.add(node);
        
        node = new DefaultMutableTreeNode(outputNodesWindow);
        InferenceNodes.add(node);
        
//        node = new DefaultMutableTreeNode(graph);
//        InferenceNodes.add(node);
        
        node = new DefaultMutableTreeNode(toleranceWindow);
        InferenceNodes.add(node);
        
        node = new DefaultMutableTreeNode(runWindow);
        InferenceNodes.add(node);
        
    }
    public static void SetDBNName(String name)
    {
        DefaultMutableTreeNode node = searchNode(DBNName, rootNode);
        
        node.setUserObject(name);
        DBNName = name;
        treeModel.reload();
    }
    public static void ResetSTDforModelParameters()
    {
        
        double stepsize = GeneralSettingsWindow.GetSetting().GetStepSize();
        double tmstp = timestep.GetTimestep();
        ArrayList<ModelParameterNode> nodes = GetAllModelParameterNodes();
        for (ModelParameterNode node : nodes)
        {
            node.SetStd(tmstp, stepsize);
        }
    }
    public static void OpenRunInferenceWindow()
    {
        if (!BlockRunWindow)
        { 
            runWindow.ResetLabel();
            ChangeView(runWindow);
        }
        else
        {
             JOptionPane.showMessageDialog(null, "Generating DBN Structure, please wait.",
                     "Generating DBN Structure", JOptionPane.INFORMATION_MESSAGE);
        }
    }
    public static String RunInference()
    {
        
        String outputFile = "";
        ArrayList<ModelParameterNode> modelParameters = GetAllModelParameterNodes();
        ArrayList<ModelVariableNode> modelVariables = GetAllModelVariableNodes();
        ArrayList<InputNode> modelInputs = GetAllInputNodes();
        ArrayList<Constant> constants = GetAllConstants();
        ArrayList<EquationInfo> equations = GetAllEquations();
        String name = DBNName;
        if (CheckMathematicalModel())
        {
        try
        {
        MainForm.MainFormInner.SetWaitCursor();
        if (GeneralSettingsWindow.isFixedTimestep())
        {
            fixedTimestepInference = new RunInferenceFixedTimestep();
            if (generateDBN == null)
                generateDBN = new  Lisp.GenerateDBN();
                
           generateDBN.CreateDBN(modelParameters, modelVariables, modelInputs, 
                equations, constants, name);    
           
            outputFile = fixedTimestepInference.RunInferenceOnDBN(modelParameters, modelVariables, modelInputs, equations,
                    constants, name, GeneralSettingsWindow.GetSetting(), timestep);
        }
        else 
        {
            double tolerance = toleranceWindow.GetTolerance().GetTolerance();
            adaptiveTimestepInference = new RunInferenceAdaptiveTimestep();
            if (generateDBN ==  null)
                generateDBN = new Lisp.GenerateDBN();
             generateDBN.CreateDBN(modelParameters, modelVariables, modelInputs, 
                equations, constants, name); 
             outputFile = adaptiveTimestepInference.RunInferenceOnDBN(modelParameters, modelVariables, modelInputs, equations, 
                     constants, name, GeneralSettingsWindow.GetSetting(), timestep, tolerance);
        }
       
        }
        catch (Throwable e)
        {
            outputFile = e.getMessage();
             //JOptionPane.showMessageDialog(null , e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
        }
        finally
        {
            MainForm.MainFormInner.SetDefaultCursor();
        }
        }
        else
        {
            outputFile = "No Mathematical Model Entered";
        }
        return outputFile;
        
    }
    private static boolean CheckMathematicalModel()
    {
        if (GlobalLists.EQUATIONS.isEmpty())
        {
//            JOptionPane.showMessageDialog(null, 
//                    "Please enter a Mathematical Model", "No Equations", JOptionPane.ERROR_MESSAGE);
            return false;
        }
        else
        {
            return true;
        }
            
    }
    public static String GenerateDBN()
    {
        String dotFilepath = "";
        ArrayList<ModelParameterNode> modelParameters = GetAllModelParameterNodes();
        ArrayList<ModelVariableNode> modelVariables = GetAllModelVariableNodes();
        ArrayList<InputNode> modelInputs = GetAllInputNodes();
        ArrayList<Constant> constants = GetAllConstants();
        ArrayList<EquationInfo> equations = GetAllEquations();
        
       
        if (CheckMathematicalModel())
        {
        try
        {
            MainForm.MainFormInner.SetWaitCursor();
            if (generateDBN == null)
                generateDBN = new  Lisp.GenerateDBN();
                
            dotFilepath = TreeStatic.generateDBN.CreateDBN(modelParameters, modelVariables, modelInputs, 
                equations, constants, DBNName);    
           
        }
        catch (Throwable e)
        {
           //  JOptionPane.showMessageDialog(null , e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
             dotFilepath = e.getMessage();
        }
        finally
        {
            MainForm.MainFormInner.SetDefaultCursor();
        }
        }
         else
        {
            dotFilepath = "No Mathematical Model Entered";
        }
       return dotFilepath;
    }
    
    private void AddMouseListener(){
        tree.addMouseListener ( new MouseAdapter ()
    {
        public void mousePressed ( MouseEvent e )
        {
            if ( SwingUtilities.isRightMouseButton ( e ) )
            {
                RightClick(e);
            }
            if (SwingUtilities.isLeftMouseButton(e) && e.getClickCount() == 2)
            {
                DoubleClick(e);                
            }
        }
    } );
    
    }
    
    
    public static void SaveProject()
    {     
        JFileChooser chooser = new JFileChooser();
        chooser.setFileFilter(new ProjectFileFilter());
        chooser.setDialogTitle("Save Project");
        int returnVal = chooser.showSaveDialog(null);
        if(returnVal == JFileChooser.APPROVE_OPTION) 
        {
       
                String filePath = chooser.getSelectedFile().getPath();
                if (ProjectFileFilter.isPrftFile(chooser.getSelectedFile()) == false)   
                {
                    filePath = filePath + ".prft";
                    
                }
               
            Project.ProjectInner.saveAsFile(filePath);
            SetDBNName(chooser.getSelectedFile().getName().replaceAll(".prft", ""));
        }
        
    }
    
   
    
    public static void LoadProject()
    {
        JFileChooser chooser = new JFileChooser();
        chooser.setFileFilter(new ProjectFileFilter());
        chooser.setDialogTitle("Load Project");
        int returnVal = chooser.showOpenDialog(null);
        if(returnVal == JFileChooser.APPROVE_OPTION) 
        {
          

            String path = chooser.getSelectedFile().getPath(); 
            String name  = chooser.getSelectedFile().getName() ;
            name = name.replaceAll(".prft", "");
            
            ClearExistingWork();    
            SetDBNName(name);
            Project.ProjectInner.openFile(path);
        }
        
        MainForm.MainFormInner.OpenHomeScreen();
    }
    
    public static void ClearExistingWork()
    {
        if(CheckMathematicalModel())
        {
          int dialogResult = JOptionPane.showConfirmDialog (null, "Would you like to save the "
                    + "current project?","Warning",JOptionPane.YES_NO_OPTION);
            if (dialogResult == JOptionPane.YES_OPTION)
            {
                SaveProject();
            }
        }
        SetDBNName("Untitled");
        GlobalLists.RemoveEverything();
        clear();
        ReloadTimestepWindow(1);
        GeneralSetting setting = new GeneralSetting();
        ReloadSettingsWindow(setting);
        createNodes(rootNode);
        evidenceWindow = new RunInference.EvidenceUploadWindow.EvidenceUploadWindowInner();
        outputNodesWindow = new RunInference.OutputNodes.OutputNodesInner();
        Tolerance t = new Tolerance();
        //t.SetTolerance(0.01); //default 
        toleranceWindow = new RunInference.ToleranceWindow.ToleranceWindowInner();
       toleranceWindow.SetTolerance(t);
        Tree.TreeStatic.OpenInferenceNodes();
        Tree.TreeStatic.OpenResultsNodes();
    }
    public static void ReloadProjectName(String name)
    {
        SetDBNName(name);
    }
    public static String GetDBNName()
    {
        return DBNName;
    }
    public static void ReloadTimestepWindow(double ts)
    {
        timestep = new Timestep.TimestepInner(ts);
    }
    
    public static void ReloadSettingsWindow(GeneralSetting st)
    {
        GeneralSettingsWindow.SetSetting(st);
    }
    
    public static GeneralSetting GetGeneralSetting()
    {
        return GeneralSettingsWindow.GetSetting();
    }
    
    public static boolean isNewProject()
    {
        if (!ProjectPath.equals(""))
        {
            File f = new File(ProjectPath);
            if(f.exists() && !f.isDirectory())
            {
                return false;
            }
        }
        return true;
    }
    static void ReloadToleranceWindow(Tolerance t) 
    {
        toleranceWindow.SetTolerance(t);
    }
    
     public static void NextScreen(JPanel CurrentScreen)
    {
        
    }
    public static void PreviousScreen(JPanel CurrentScreen)
    {
        
    }

    }
}
   