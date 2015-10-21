/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Lisp;

import Nodes.Constant;
import Nodes.InputNode;
import Nodes.ModelParameterNode;
import Nodes.ModelVariableNode;
import Nodes.StatisticalDistributions.LinearGaussian;
import Nodes.StatisticalDistributions.StatisticalDistribution;
import Nodes.StatisticalDistributions.TruncatedGaussian;
import Nodes.StatisticalDistributions.UniformDistribution;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;


/**
 *
 * @author Administrator
 */
public class AIMAGenerateDBN {
    public static class AimaFunctionBuilder{
        private static String dbn_name ;
        LispConnector.StaticLispConnector lisp;
        
        
        private String GetDistributionString(StatisticalDistribution dist)
        {
            String nodeType = "";
            if (dist.getClass().getName().equals("Nodes.StatisticalDistributions.TruncatedGaussian"))
            {
             nodeType = ":truncated-gaussian";   
            }
            else if (dist.getClass().getName().equals("Nodes.StatisticalDistributions.UniformDistribution"))
            {                    
                nodeType = ":uniform";
            }
            else if (dist.getClass().getName().equals("Nodes.StatisticalDistributions.LinearGaussian"))
            {                    
                nodeType = ":linear-gaussian";
            }
            return nodeType;
        }
        public String CreateModelCoeff(ModelParameterNode node)
        {
            
            String name = node.GetName();
            String nodeType = "";
            StatisticalDistribution dist = node.GetDistribtion();
            nodeType = GetDistributionString(dist);
            String command = "(create-model-coefficients '(" + name + ") " + dbn_name + " " + nodeType + ")";

            return command;             
            
        }
        
        public String CreateModelInput(InputNode node)
        {
            //(create-model-inputs '(Uex D PN) icing-dbn)
            String name = node.toString();
            String command = "(create-model-inputs '(" + name + ") " + dbn_name + ")";            
            return command;
        }
        public String CreateTrueValueNode(ModelVariableNode node)
        {
            //(create-true-value-node '(BG Q I P1 P2) icing-dbn)
            String name = node.toString();
            String command = "(create-true-value-node '(" + name + ") " + dbn_name + ")";
            return command;
        }
        public String AddParents(String parentName, ArrayList<String> children)
        {
            //(add-parents icing-dbn '|deltaBG| (list  'Pg 'EGPb 'Q 'Si 'P2 'PN)) 
            String command = "(add-parents " + dbn_name + " '|delta" + parentName + "| (list "; 
            for (String node : children)
            {
                command = command + "'" + node + " ";
            }
            command = command + "))";
            return command;
        }
        public String AddNode(ModelParameterNode node)
        {
            //(add-node icing-dbn 'Observed-BG :linear-gaussian)  
            String name = node.GetName();
            String nodeType = GetDistributionString(node.GetDistribtion());
            String command = "(add-node " + dbn_name + " '" + name
                    + " " + nodeType + ")";
            return command;
        }
        public String SetDBNName(String name)
        {
            dbn_name = name;
            String command = "(setq "+ name + " (make-dbn))";
            return command;
        }
        public String SortDBN()
        {
            //(sort-dbn icing-dbn) 
            String command = "(sort-dbn " + dbn_name + ")";
            return command;
        }
        
        public String AddCPT(String name, String lambda)
        {
            //  (setf (deterministic-bnode-function (bnode-by-name (subscript vnodename 0) icing-dbn)) 
              //'(lambda () 0)
              //)
            String command = "(setf (deterministic-bnode-function (bnode-by-name " +
                    name + " " + dbn_name + "))";
            command = command + " '" + lambda + ")";
            return command;
        }
        
        public String Subscript(String name, int subs)
        {
            //String command = name + "_" + String.valueOf(subs);
           String command = "(subscript '" + name + " " + String.valueOf(subs) + ")";  //(subscript vnodename 1))
           return command;
        }
        
        public String LambdaForModelVar0 (ModelVariableNode node)
        {
            return "(lambda () " + String.valueOf(node.GetInitialValue()) +")";
            
        }
        
        public String LambdaForModelVar1 ()
        {
            String command = "(lambda (delta_0 var_0) (+ var_0 (* deltastep delta_0) ))";
            return command;
        }
        public String LambdaForDelta(ArrayList<String> terms, String exp)
        {
            String command = "(LAMBDA ";
            command = command  + "(";
            for (String term : terms)
            {
                command = command + term + " ";
            }
            command = command +  ")";
            command = command + exp;
            command = command + ")";
            return command;
        }
        
        public String CreateParameterCPTS(ModelParameterNode node)
        {
            StatisticalDistribution dist = node.GetDistribtion();
            String command = "";
            if (dist.getClass().getName().equals("Nodes.StatisticalDistributions.TruncatedGaussian"))
            {
                command = CreateTruncGaussParameterCPTS(node);
            }
            else if (dist.getClass().getName().equals("Nodes.StatisticalDistributions.UniformDistribution"))
            {
                command = CreateUniformParameterCPTS(node);
            }
            else if(dist.getClass().getName().equals("Nodes.StatisticalDistributions.LinearGaussian"))
            {
                command = CreateLinearGaussParameterCPTS(node);
            }
            return command;
        }
        private String CreateTruncGaussParameterCPTS(ModelParameterNode node)
        {
            BigDecimal std = new BigDecimal(node.GetStd());
           
            std = std.setScale(5, RoundingMode.HALF_EVEN);
            TruncatedGaussian dist = (TruncatedGaussian)node.GetDistribtion();
            //mean std lowerlimit upperlimit
            //(create-parameter-cpts icing-dbn 'Si 0.0003 0.001 0 .001) 
            String command = "(create-truncated-gaussian-parameter-cpts ";
            command = command + dbn_name;
            command = command + " '" + node.GetName() + " ";
            command = command + String.valueOf(dist.GetMean()) + " ";
            command = command + String.valueOf(dist.GetStd()) + " ";
            command = command + String.valueOf(dist.GetLowerLimit()) + " ";
            command = command + String.valueOf(dist.GetUpperLimit()) + " ";//)";
            command = command + node.GetStd() + " )";
            
            return command;
        }
        private  String CreateUniformParameterCPTS(ModelParameterNode node)
        {
            UniformDistribution dist= (UniformDistribution)node.GetDistribtion();
            String command = "(create-uniform-parameter-cpts " + dbn_name + " '";
            command = command + node.GetName() + " ";
            command = command + String.valueOf(dist.GetLowerLimit()) + " ";
            command = command + String.valueOf(dist.GetUpperLimit()) + " ";
            command = command + ")";
            return command;
            //Pg 0.001 0.1 )"
        }
        private  String CreateLinearGaussParameterCPTS(ModelParameterNode node)
        {
            BigDecimal std = new BigDecimal(node.GetStd());
             std = std.setScale(5, RoundingMode.HALF_EVEN);
             LinearGaussian dist = (LinearGaussian)node.GetDistribtion();
            
            String command = "(linear-truncated-gaussian-parameter-cpts ";
            command = command + dbn_name;
            command = command + " '" + node.GetName() + " ";
            command = command + String.valueOf(dist.GetMean()) + " ";
            command = command + String.valueOf(dist.GetStd()) + " ";
            command = command + node.GetStd() + " )";
            
            return command;
        }
        
        public String CreateInputCPTS(InputNode node)
        {
            String command = "";
            if (node.IsObserveableNode())
            {
                command = CreateEvidenceCPT(node);
            }
            else
            {
                //(dbn parm mean std parentStd parentCoeff parentOffset)
                command = "(create-input-cpts ";
                command = command + dbn_name;
                command = command + " '";
                command = command + node.toString() + " ";
                command = command + String.valueOf(node.GetMean()) + " ";
                command = command + String.valueOf(node.GetStd()) + " ";
                command = command + String.valueOf(node.GetParentStd()) + " ";
                command = command + String.valueOf(node.GetParentCoefficient()) + " ";
                command = command + String.valueOf(node.GetParentOffset()) + " )";
            }
            return command;
           //  (create-input-cpts icing-dbn 'Uex 100 100) 
        }
        
        public String CreateEvidenceCPT(InputNode node)
        {//(dbn parm std parentCoeff parentOffset)
            String command = "(create-evidence-cpts ";
            command = command + dbn_name + " '";
            command = command + node.toString() + " ";
            command = command + String.valueOf(node.GetParentStd()) + " ";
            command = command + String.valueOf(node.GetParentCoefficient()) + " ";
            command = command + String.valueOf(node.GetParentOffset()) + " )";
            return command;
        }
        public String SaveDBN(String path)
        {
            String command = "(save-bn ";
            command = command + dbn_name + " " + path + ")";
            return command;
        }
        public String DotDBN(String path)
        {
            String command = "(dot-dbn " + dbn_name + " " + path + ")";
            return command;
        }

        
        public String CreateEvidenceNodes(InputNode node) 
        {
            /*
            (create-evidence-nodes 'Observed-X '(X) hello)
            */
            String name = node.toString();
            String parentName = node.GetEvidenceParent().GetName();
            
            String command = "(create-evidence-nodes '" + name + "  '(" + parentName + ") " +
                    dbn_name +" )";            
            return command;
        }
             
        
        
}
    
}
