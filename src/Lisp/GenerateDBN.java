/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Lisp;

//import Graphviz.Proba;
import Nodes.Constant;
import Nodes.EquationInfo;
import Nodes.InputNode;
import Nodes.ModelParameterNode;
import Nodes.ModelVariableNode;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import javax.swing.JOptionPane;

/**
 *
 * @author Administrator
 */
public class GenerateDBN {
    
    ArrayList<ModelParameterNode> modelParameterNodes = new ArrayList<>();
    ArrayList<ModelVariableNode> modelVariableNodes = new ArrayList<>();
    ArrayList<InputNode> inputNodes= new ArrayList<>();
    ArrayList<Constant> constants = new ArrayList<>();
    ArrayList<EquationInfo> equations = new ArrayList<>();
    String dbnName = "" ; 
    AIMAGenerateDBN.AimaFunctionBuilder function = new AIMAGenerateDBN.AimaFunctionBuilder();
    
    
    public GenerateDBN()
    {
        SetupLisp();
        
    }
    private void SetupLisp()
    {
        LispConnector.StaticLispConnector.Setup();
        
    }
    
    public String CreateDBN(ArrayList<ModelParameterNode> modelParams, 
            ArrayList<ModelVariableNode> modelVars,
            ArrayList<InputNode> inputNodes,
            ArrayList<EquationInfo> equations,             
            ArrayList<Constant> constants,
            String dbnName)
    {
        this.modelParameterNodes = modelParams;
        this.modelVariableNodes = modelVars;
        this.inputNodes = inputNodes;
        this.constants = constants;
        this.equations = equations;
        this.dbnName = dbnName;
       LispConnector.StaticLispConnector.SetDebuggerHook();
       CreateResultsFolder();
       
        SetDBNName();
        CreateModelCoefficients();
        CreateModelInputs();
        CreateTrueValueNodes();
        CreateEvidenceNodes();
        AddDeltaParents();
        //LispConnector.StaticLispConnector.execute("(print (mapcar #'bnode-name (bnode-parents (bnode-by-name '|deltaX_1| " + dbnName + "))))");
        SortDBN();
       // LispConnector.StaticLispConnector.execute("(print (mapcar #'bnode-name (bnode-parents (bnode-by-name '|deltaX_1| " + dbnName + "))))");
        AddCPTS();
        SaveDBN();
        // LispConnector.StaticLispConnector.execute("(print (mapcar #'bnode-name (bnode-parents (bnode-by-name '|deltaX_1| " + dbnName + "))))");
    
        String dotFile = DotDBN();
        // LispConnector.StaticLispConnector.execute("(print (mapcar #'bnode-name (bnode-parents (bnode-by-name '|deltaX_1| " + dbnName + "))))");
    
         //LispConnector.StaticLispConnector.execute("stoe");
        return dotFile;
        
    }
    private void CreateResultsFolder()
    {
        String folderNames[] = {"DBN", "DOT", "DBNImages", "Outputs", "Temp"};
        
        for (String name : folderNames)
        {
            String path = System.getProperty("user.dir");
                // 
            path = path.replace("\\", "/");
            name = path + "/Results/" + name;
            File theDir = new File(name);
            theDir.mkdirs();
           
        }
        
    }
    private void SaveDBN()
    {
        String path = System.getProperty("user.dir");
       // 
        path = path.replace("\\", "/");
        path = path + "/Results/DBN/" + dbnName + ".dbn" ;
        path = "\"" + path + "\"";
        LispConnector.StaticLispConnector.execute(function.SaveDBN(path));
    }
    private String DotDBN()
    {
        String path = System.getProperty("user.dir");
       // 
        path = path.replace("\\", "/");
        path = path + "/Results/DOT/" + dbnName ;
        
        
        String temp = "\"" + path + "\"";
        LispConnector.StaticLispConnector.execute(function.DotDBN(temp));
        return path;
        //ConvertDOTtoPNG();
    }
    private void SetDBNName()
    {
        LispConnector.StaticLispConnector .execute(function.SetDBNName(dbnName));
    }
    private ArrayList<String> GetChildren(EquationInfo eq)
    {
        ArrayList<String> Children = new ArrayList<>();
        for (String child : eq.GetTimeDependentVariables())
            {
                Children.add(child);
            }
            for (String child : eq.GetTimeIndependentVariables())
            {
                boolean dontAdd = false;
                for (Constant c : constants)
                {
                    if (child.equals(c.toString()))
                    {
                        dontAdd = true;
                    }
                }
                if (!dontAdd)
                {
                    Children.add(child);
                }
            }
        return Children;
    }
    public void SortDBN()
    {
        LispConnector.StaticLispConnector .execute(function.SortDBN());
    }
    private void AddCPTS()
    {
        for (ModelVariableNode node : modelVariableNodes)//(EquationInfo eq : equations)// ModelVariableNode node : modelVariableNodes)
        {
            
            LispConnector.StaticLispConnector .execute(function.AddCPT(function.Subscript(node.toString(), 0), function.LambdaForModelVar0(node)));
        }
        for (EquationInfo eq : equations)//(ModelVariableNode node : modelVariableNodes)
        {
            LispConnector.StaticLispConnector .execute(function.AddCPT(function.Subscript(eq.GetName(), 1), function.LambdaForModelVar1()));
        }
        for (EquationInfo eq : equations)
        {
            //Loop for vnodename in '(|deltaBG_0|  |deltaBG_1| ) DO
            // hard code subscripts here
            //String name = "|delta" + function.Subscript(eq.GetName(), 0) + "|";
            // dont mix children and parents. they are children in Java tree structs but paretns in DBN
            String name = "'|delta" + eq.GetName().toUpperCase() + "_0|" ; 
            ArrayList<String> children= GetChildren(eq);
            Collections.reverse(children);
            LispConnector.StaticLispConnector .execute(function.AddCPT(name, function.LambdaForDelta(children, eq.GetPreFix())));
            //name =  "|delta" + function.Subscript(eq.GetName(), 1) + "|";
            name = "'|delta" + eq.GetName().toUpperCase() + "_1|" ;
            LispConnector.StaticLispConnector .execute(function.AddCPT(name, function.LambdaForDelta(children, eq.GetPreFix())));           
            
        }
        for (ModelParameterNode node : modelParameterNodes)
        {
            LispConnector.StaticLispConnector .execute(function.CreateParameterCPTS(node));
        }
        for (InputNode node : inputNodes)
        {
            LispConnector.StaticLispConnector .execute(function.CreateInputCPTS(node));
        }
        
    }
    private void CreateModelCoefficients()
    {
        for(ModelParameterNode node : modelParameterNodes)
        {
            LispConnector.StaticLispConnector .execute(function.CreateModelCoeff(node));
        }
    }
    private void CreateModelInputs()
    {
        for (InputNode node : inputNodes)
        {
            if (!node.IsObserveableNode())
            {
                LispConnector.StaticLispConnector .execute(function.CreateModelInput(node));
            }
        }
    }
    private void CreateTrueValueNodes()
    {
        for (ModelVariableNode node  : modelVariableNodes)
        {
            LispConnector.StaticLispConnector .execute(function.CreateTrueValueNode(node));
        }
    }
    private void AddDeltaParents()
    {
        for (EquationInfo eq : equations)
        {
            String name = eq.GetName();
            ArrayList<String> Children = GetChildren(eq);
            LispConnector.StaticLispConnector .execute(function.AddParents(name, Children));
        }
    }
    private void CreateEvidenceNodes()
    {
        for (InputNode node :  inputNodes)
        {
            if (node.IsObserveableNode())
            {
                LispConnector.StaticLispConnector .execute(function.CreateEvidenceNodes(node));
            }
        }
    }
       
    
}
