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
public class RunInferenceAdaptiveTimestep {
    ArrayList<ModelParameterNode> modelParameterNodes = new ArrayList<>();
    ArrayList<ModelVariableNode> modelVariableNodes = new ArrayList<>();
    ArrayList<InputNode> inputNodes= new ArrayList<>();
    ArrayList<Constant> constants = new ArrayList<>();
    ArrayList<EquationInfo> equations = new ArrayList<>();
    String dbnName = "" ; 
    AIMAAdaptiveTimestepInference.AimaFunctionBuilder function = new AIMAAdaptiveTimestepInference.AimaFunctionBuilder();
    GeneralSetting setting;
    double timestep = 1;
    double summary_interval = 1;
    double tolerance = 0.01;
    //static LispConnector.StaticLispConnector lisp;
    
    
    public String RunInferenceOnDBN(ArrayList<ModelParameterNode> modelParams, 
            ArrayList<ModelVariableNode> modelVars,
            ArrayList<InputNode> inputNodes,
            ArrayList<EquationInfo> equations,             
            ArrayList<Constant> constants,
            String dbnName,
            GeneralSetting setting,
            Timestep.TimestepInner timestep,
            double tolerance)
    {
        this.modelParameterNodes = modelParams;
        this.modelVariableNodes = modelVars;
        this.inputNodes = inputNodes;
        this.constants = constants;
        this.equations = equations;
        this.dbnName = dbnName;
        this.setting = setting;
        this.timestep = timestep.GetTimestep();
        this.summary_interval = setting.GetSummaryIntervals();
        this.tolerance = tolerance;
        LispConnector.StaticLispConnector.SetDebuggerHook();
        
        DefineDBN();
        AddParentIndices();
        DefineParameters();
        SetModelConstants();
        
        DefineNodeList();
        DefineToleranceList();
        InitializeTotalTimestepsVector();
        DeclareEvidenceArrays();
        //
       //LispConnector.StaticLispConnector.execute("e");
        SetupEvidence();
        
        RunAdaptiveParticleFilterInference();
        WriteOutputFile();
        String name = function.GetOutputFileName(setting.GetSampleCount());
        return name;
      
    }
    
    private void DeclareEvidenceArrays()
    {
        int continousCount = 0;
        int instantCount = 0;
        for (InputNode node : inputNodes)
        {
            if (node.GetEvidence().GetFile() != null)
            {
                if (node.GetEvidence().isContinous())
                    continousCount++;
                else
                    instantCount++;
            }
        }
        LispConnector.StaticLispConnector.execute(
                function.DefineContinousEvidenceArray(continousCount));
        LispConnector.StaticLispConnector.execute(
                function.DefineInstantaneousEvidenceArray(instantCount));
    }
     private void SetModelConstants()
    {
        for (Constant constant : constants)
        {
            LispConnector.StaticLispConnector.execute(function.DefParameter(constant));
        }
    }
    private void AddParentIndices()
    {
        LispConnector.StaticLispConnector.execute(function.AddParentIndices());
    }
    private void DefineDBN()
    {
        LispConnector.StaticLispConnector.execute(function.DefineDBN(dbnName));
    }
    private void DefineParameters()
    {
        /*
          (defparameter timestep sum-interval)
  (defparameter deltastep 1)
  (defparameter origtimestep 1)
        */
        LispConnector.StaticLispConnector.execute(function.DefParameter("timestep", timestep));
        LispConnector.StaticLispConnector.execute(function.DefParameter("deltastep", summary_interval));
        LispConnector.StaticLispConnector.execute(function.DefParameter("origtimestep", timestep));
        LispConnector.StaticLispConnector.execute(function.InitializeSummaryInterval(summary_interval));
        LispConnector.StaticLispConnector.execute(function.InitializeDefinedStep(timestep));
        LispConnector.StaticLispConnector.execute(function.InitializeFinishTime(setting.GetTimespan()));
        LispConnector.StaticLispConnector.execute(function.InitializeWeightNodesToNil());
        LispConnector.StaticLispConnector.execute(function.InitializeTolerance(tolerance));
        LispConnector.StaticLispConnector.execute(function.InitializeNumberOfSamples(setting.GetSampleCount()));
        LispConnector.StaticLispConnector.execute(function.DefParameter("totalnosteps", 0));
        LispConnector.StaticLispConnector.execute(function.DefParameter("rejectcount", 0));
        
        LispConnector.StaticLispConnector.execute(function.DefParameter("increasedcount", 0));
        LispConnector.StaticLispConnector.execute(function.DefParameter("avgsteps", 0));
        LispConnector.StaticLispConnector.execute(function.DefParameter("NoOfComparisons", 0));
        LispConnector.StaticLispConnector.execute(function.DefParameter("NoOfComparisons1", 0));
        LispConnector.StaticLispConnector.execute(function.DefParameter("timestepcounter", -1));
        /*
        (defparameter rejectcount 0)
  (defparameter increasedcount 0)
   (defparameter avgsteps 0)
  (defparameter NoOfComparisons 0 )
  (defparameter NoOfComparisons1 0 )
  (defparameter timestepcounter -1 )
        
        */
        //continue from here
//LispConnector.StaticLispConnector.execute(function.InitializeToleranc)
    }
    private void DefineToleranceList()
    {
        LispConnector.StaticLispConnector.execute(function.DefineToleranceList(modelVariableNodes));
    }
    private void InitializeTotalTimestepsVector()
    {
        LispConnector.StaticLispConnector.execute(function.DefineTotalSteps());
    }
    private void SetupEvidence()
    {
        int instantEvidenceCounter = 0;
        int ContinousEvidenceCounter = 0;
        for (InputNode node : inputNodes)
        {
            if (node.GetEvidence().GetFile() != null)
            {
                if (node.GetEvidence().isContinous())
                    LispConnector.StaticLispConnector.execute(function.ReadInEvidence(node, ContinousEvidenceCounter++));
                else
                    LispConnector.StaticLispConnector.execute(function.ReadInEvidence(node, instantEvidenceCounter++));
            }
        }
    }
    private void DefineNodeList()
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
    }
    private void RunAdaptiveParticleFilterInference()
    {
        LispConnector.StaticLispConnector.execute(function.RunAdaptiveParticleFilter(
                setting.GetSampleCount()));
    }
    private void WriteOutputFile()
    {
        LispConnector.StaticLispConnector.execute(function.WriteOutputFile(
                setting.GetSampleCount(), summary_interval));
    }
    
}
