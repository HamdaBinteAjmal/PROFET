/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Lisp;

import Nodes.Constant;
import Nodes.InputNode;
import Nodes.ModelVariableNode;
import java.util.ArrayList;

/**
 *
 * @author Administrator
 */
public class AIMAAdaptiveTimestepInference {
     public static class AimaFunctionBuilder{
        private static String dbn_name ;//= "Hamda"; //change later
       // LispConnector.StaticLispConnector lisp;
        
         public String DefineDBN(String name)
        {
            dbn_name = name;
            String path = System.getProperty("user.dir");
            path = path.replace("\\", "/");
            path = path + "/Results/DBN/" + dbn_name + ".dbn" ;
            
            String command = "(DEFPARAMETER " + dbn_name + "(sort-dbn (load-bn \"" 
                    + path + "\" )))";
            return command;
            
        }
          public String AddParentIndices()
        {
            //(add-parent-indexes insulin-dbn)
            String command = "(add-parent-indexes " + dbn_name + ")";
            return command;
        }
           public String DefParameter(String parameter, double value)
        {
            String command = "(defparameter ";
            command = command + parameter + " ";
            command = command + String.valueOf(value);
            command = command + " )";
            return command;
        }
            public String DefParameter(Constant c)
        {
            String command = "(defparameter ";
            command = command + c.toString() + " ";
            command = command + String.valueOf(c.GetValue());
            command = command + " )";
            return command;
        }
           ///Additional functions for adaptive timestep inference
        public String InitializeSummaryInterval(double interval)
        {
            String command = "(setf summary-interval " + String.valueOf(interval) + " )";
            return command;
        }
        public String InitializeDefinedStep(double step)
        {
            String command = "(setf defined-step " + String.valueOf(step) + " )";
            return command;
        }
         public String InitializeFinishTime(double time)
        {
            String command = "(setf finish-time " + String.valueOf(time) + " )";
            return command;
        }
        public String DefineToleranceList(ArrayList<ModelVariableNode> list)
        {
            String command = "(setf tolerance-list (list ";
            for (ModelVariableNode node: list)
            {
                if (node.ImposeTolerance())
                {
                    command = command + "(bnode-index (bnode-by-name '";
                    command = command + "|delta" + node.GetName() + "_0| ";
                    command = command + dbn_name;
                    command = command + "))";
                }
            }
            command = command + "))";
            return command;         
        }
        public String InitializeTolerance(double tolerance)
        {
            String command = "(setf tolerance " + String.valueOf(tolerance) + ")";
            return command;
        }
       public String InitializeNumberOfSamples(double count)
       {
           int counti = (int)count;
           String command = "(setf no-of-samples " + String.valueOf(counti) + ")";
            return command;
       }
       public String DefineTotalSteps()
       {
           String command =  "(defparameter totalsteps (make-sequence 'vector no-of-samples :initial-element 0))";
           return command;
       }
       public String DefineContinousEvidenceArray(int count)
       {
           String command = "(defparameter continuous-evidence (make-sequence 'vector "
                   + String.valueOf(count) + "))";
           return command;
       }
       public String DefineInstantaneousEvidenceArray(int count)
       {
           String command = "(defparameter instant-evidence (make-sequence 'vector "
                   + String.valueOf(count) + "))";
           return command;
       }
        public String Subscript(String name, int subs)
        {
            //String command = name + "_" + String.valueOf(subs);
           String command = "(subscript '" + name + " " + String.valueOf(subs) + ")";  //(subscript vnodename 1))
           return command;
        }
       public String ReadInEvidence(InputNode node, int column)
       {
           String command = "";
            String nodename = Subscript(node.toString(), 0);
            String filepath = node.GetEvidence().GetFile().getAbsolutePath();
            filepath = filepath.replace("\\", "/");
            if(node.GetEvidence().isContinous())
            {
                command = ReadInContinousEvidence(filepath, column, nodename);
            }
            else
            {
                command = ReadInInstantaneousEvidence(filepath, column, nodename);
            }
            return command;
       }
       private String ReadInContinousEvidence(String filename, int column, String nodename)
       {
           String command = "(read-in-continous-evidence-adaptive \"" ;
           command = command + filename + "\" " + String.valueOf(column) ;
           command = command + " " + nodename;
           command = command + " " + dbn_name;
           command = command + " )";
           return command;
        }
       private String ReadInInstantaneousEvidence(String filename, int column, String nodename)
       {
           String command = "(read-in-instantaneous-evidence-adaptive \"" ;
           command = command + filename + "\" " + String.valueOf(column) ;
           command = command + " " + nodename;
           command = command + " " + dbn_name;
           command = command + " )";
           return command;
       }
        public String DefineNodelist(ArrayList<String> nodenames)
        {
            String command = "(setf nodelist (list " ;//'BG 'Q 'I 'P1 'P2 'Si 'Pg 'EGPb 'ni 'Uex 'intended-Uex 'D 'PN))";
            for (String nodename : nodenames)
            {
                command = command + "'" + nodename + " ";
            }
            command = command + "))";
            return command;
        }
        public String RunAdaptiveParticleFilter(double sampleCount)
        {
            int counti = (int)sampleCount;
            String command = "(setf output (adaptive-pf-by-name nodelist ";
            command = command + dbn_name + " summary-interval finish-time ";
            command = command + ":N " + String.valueOf(counti) + " ";
            command = command + ":tolerance-node-list tolerance-list ";
            command = command + ":tolerance tolerance ";
            command = command + ":defined-step defined-step))";
            
            return command;
        }
        
        public String WriteOutputFile(double samples, double summaryInterval)
        {
            int samplesi = (int)samples;
            String command = "(with-open-file (s \"" + GetOutputFileName(samplesi) ;
            command = command + "\" :direction :output :if-does-not-exist :create :if-exists :supersede)\n ";
            command = command + "(format s \"Time ~{~S std ~}\" nodelist)\n ";
            command = command + " (fresh-line s)\n";
            command = command + "(LOOP FOR i FROM 0 TO (1- (length (third output))) DO  \n" +
"                                  ;;(format s \"~15,7F\" (fifth (first (subseq (third output) i (+ 1 i)))))\n" +
"                                  (prin1 (fifth (first (subseq (third output) i (+ 1 i)))) s)\n" +
"                                  (princ \" \" s)\n";
            command = command + "(LOOP FOR j FROM 0 TO (1- (length nodelist)) DO\n" +
"                                        ;;mean and standard deviation\n" +
"                                        (format s \"~15,7F\" (first (elt (first (first (subseq (third output) i (+ 1 i)))) j)) )\n" +
"                                        (princ \" \" s)\n" +
"                                        (format s \"~15,7F\" (second (elt (first (first (subseq (third output) i (+ 1 i)))) j)) )\n" +
"                                        (princ \" \" s))\n";
            command = command + " (fresh-line s)\n" +
"                                  (fresh-line s)";
            command = command + " ))";
            return command;
        }
       
        public String GetOutputFileName(double samples)
        {
            int samplesi = (int)samples;
           
            String path = System.getProperty("user.dir");
            path = path.replace("\\", "/");
            path = path + "/Results/Outputs/";
            
            String name = path  + dbn_name + "_" + String.valueOf(samplesi)
                    + "_"  + "adaptive" + ".csv";
            return name;
            
        }
        public String InitializeWeightNodesToNil()
        {
            String command = "(setf weight-nodes nil)";
            return command;
        }
     }
    
}
