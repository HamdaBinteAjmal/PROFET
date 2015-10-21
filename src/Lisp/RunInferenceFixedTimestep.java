/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Lisp;

import MainInterface.Timestep;
import Nodes.Constant;
import Nodes.EquationInfo;
import Nodes.InputNode;
import Nodes.ModelParameterNode;
import Nodes.ModelVariableNode;
import RunInference.GeneralSetting;
import RunInference.GeneralSettingsWindow;
import java.util.ArrayList;

/**
 *
 * @author Administrator
 */
public class RunInferenceFixedTimestep {
    ArrayList<ModelParameterNode> modelParameterNodes = new ArrayList<>();
    ArrayList<ModelVariableNode> modelVariableNodes = new ArrayList<>();
    ArrayList<InputNode> inputNodes= new ArrayList<>();
    ArrayList<Constant> constants = new ArrayList<>();
    ArrayList<EquationInfo> equations = new ArrayList<>();
    String dbnName = "" ; 
    AIMAFixedTimestepInference.AimaFunctionBuilder function = new AIMAFixedTimestepInference.AimaFunctionBuilder();
    GeneralSetting setting;
    double timestep = 1;
   
    public String RunInferenceOnDBN(ArrayList<ModelParameterNode> modelParams, 
            ArrayList<ModelVariableNode> modelVars,
            ArrayList<InputNode> inputNodes,
            ArrayList<EquationInfo> equations,             
            ArrayList<Constant> constants,
            String dbnName,
            GeneralSetting setting,
            Timestep.TimestepInner timestep)
    {
       
        this.modelParameterNodes = modelParams;
        this.modelVariableNodes = modelVars;
        this.inputNodes = inputNodes;
        this.constants = constants;
        this.equations = equations;
        this.dbnName = dbnName;
        this.setting = setting;
        this.timestep = timestep.GetTimestep();
        LispConnector.StaticLispConnector.SetDebuggerHook();
        DefineDBN();
        SetModelConstants();
        SetOtherParameters();
        AddParentIndices();
        DefineVarsPerSlice();
        DefineNumberOfSlices();
        DeclareEvidenceArrays();
        ReadInEvidence();
        // LispConnector.StaticLispConnector.execute("(print evidence)");
        InitializeOutputVector();
        FillEvidenceVector();
        RunFixedTimestepInference();
        WritePredictionOutputFile();
        String name = function.GetOutputFileName(setting.GetSampleCount());
        return name;
    }
   
    private void WritePredictionOutputFile()
    {
        
        LispConnector.StaticLispConnector.execute(function.WriteOutputFile(
                (int)setting.GetSampleCount()));
    }
    private void RunFixedTimestepInference()
    {
        ArrayList<String> list = new ArrayList<>();
        for (InputNode node : inputNodes)
        {
            if (node.IsOutputRequired())
            {
                list.add(node.toString());
            }
        }
        for (ModelVariableNode node : modelVariableNodes)
        {
            if (node.IsOutputRequired())
            {
                list.add(node.toString());
            }
        }
        for (ModelParameterNode node : modelParameterNodes)
        {
             if (node.IsOutputRequired())
            {
                list.add(node.toString());
            }
        }
        
        LispConnector.StaticLispConnector.execute(function.DefineNodelist(list));
        
        LispConnector.StaticLispConnector.execute(function.RunParticleFilter((int)setting.GetSampleCount()));
        
    }
    private void FillEvidenceVector()
    {
        for(InputNode node : inputNodes)
        {
            if (!node.GetEvidence().isContinous()) //instananeous
            {
                LispConnector.StaticLispConnector.execute(function.FillInstantaneousEvidence(node, (int)timestep));
            }
            else
            {
                LispConnector.StaticLispConnector.execute(function.FillContinousEvidence(node, (int)timestep));
            }
        }
    }
    private void InitializeOutputVector()
    {
        LispConnector.StaticLispConnector.execute(function.InitializeEvidenceVector());
    }
    private void DeclareEvidenceArrays()
    {
        for (InputNode node : inputNodes)
        {
            LispConnector.StaticLispConnector.execute(function.DeclareEvidence(node));
        }
    }
    private void DefineVarsPerSlice()
    {
        LispConnector.StaticLispConnector.execute(function.InitializeVariablesPerSlice());
    }
    private void DefineNumberOfSlices()
    {
        //double slices =  setting.GetTimespan() / timestep;
        double slices =  setting.GetTimespan() / setting.GetStepSize();
        int temp = (int) slices;
        LispConnector.StaticLispConnector.execute(function.InitializeNoOfSlices(temp));
    }
    private void DefineDBN()
    {
         LispConnector.StaticLispConnector .execute(function.DefineDBN(dbnName));
    }
    private void SetModelConstants()
    {
        for (Constant constant : constants)
        {
            LispConnector.StaticLispConnector.execute(function.DefParameter(constant));
        }
    }
    private void SetOtherParameters()
    {
        //TODO : DO NOT HARD CODE
        LispConnector.StaticLispConnector.execute(function.DefParameter("TotalSqPercentError", 0));
        LispConnector.StaticLispConnector.execute(function.DefParameter("TotalAbsPercentError", 0));
                
        LispConnector.StaticLispConnector.execute(function.DefParameter("TotalTrivialError", 0));
        LispConnector.StaticLispConnector.execute(function.DefParameter("NoOfComparisons", 0));
        LispConnector.StaticLispConnector.execute(function.DefParameter("timestep", timestep));
        LispConnector.StaticLispConnector.execute(function.DefParameter("deltastep", setting.GetStepSize()));
        LispConnector.StaticLispConnector.execute(function.DefParameter("stepsize", setting.GetStepSize()));
    }
    private void AddParentIndices()
    {
        LispConnector.StaticLispConnector.execute(function.AddParentIndices());
    }
    private void ReadInEvidence()
    {
        for (InputNode node : inputNodes)
        {
            if (node.GetEvidence().GetFile() != null)
            LispConnector.StaticLispConnector.execute(function.ReadEvidenceFile(node));
        }
    }
    
    
    
    
    
}
