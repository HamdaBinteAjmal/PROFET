/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Lisp;

import Nodes.Constant;
import Nodes.InputNode;
import java.util.ArrayList;

/**
 *
 * @author Administrator
 */
public class AIMAFixedTimestepInference {
     public static class AimaFunctionBuilder{
        private static String dbn_name ;//= "Hamda"; //change later
        LispConnector.StaticLispConnector lisp;
         /// Now the functions of Inference Run
        public String DefParameter(Constant c)
        {
            String command = "(defparameter ";
            command = command + c.toString() + " ";
            command = command + String.valueOf(c.GetValue());
            command = command + " )";
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
        public String InitializeVariablesPerSlice()
        {
            //(vars-per-slice (list-length (dbn-var-names insulin-dbn)))
            String command = "(setf vars-per-slice (list-length (dbn-var-names " + dbn_name + ")))";
            return command;
        }
        public String InitializeNoOfSlices(int slices)
        {
            String slice = String.valueOf(slices);
            //(no-of-slices  (truncate (/ timespan timestep)))
            String command = "(setf no-of-slices  " + slice + ")";
            return command;
        }
        public String DeclareEvidence(InputNode node)
        {
            //(setf glucose-evidence (make-array 0 :fill-pointer t :adjustable t))
            String command = "(setf " + node.GetEvidenceName() + " (make-array 0 :fill-pointer t :adjustable t))";
            return command;
        }
        public String ReadEvidenceFile(InputNode node)
        {
            String file = node.GetEvidence().GetFile().getAbsolutePath();
            file = file.replace("\\", "/");
            String command = "(read-in-evidence-fixed \"" + file + "\" " + node.GetEvidenceName() + " )";
            return command;
        }
        public String InitializeEvidenceVector()
        {
            String command = "(setf evidence (make-sequence 'vector (* no-of-slices vars-per-slice) :initial-element NIL))";
            return command;
        }
        public String Subscript(String name, int subs)
        {
            //String command = name + "_" + String.valueOf(subs);
           String command = "(subscript '" + name + " " + String.valueOf(subs) + ")";  //(subscript vnodename 1))
           return command;
        }
        public String FillInstantaneousEvidence(InputNode node, int timestep )
        {
            String nodename = Subscript(node.toString(), 0);
            String evidenceName = node.GetEvidenceName();
            
            String command =    "(loop FOR i FROM 0 TO (1- (length " + evidenceName +")) DO\n" +
                                "(if (<=  (/ (elt (elt " + evidenceName + " i) 0) stepsize) (1- no-of-slices))\n" +
                                "(setf (aref evidence (+ (bnode-index (bnode-by-name " + nodename + " " + dbn_name + 
                                ")) (* (truncate (/ (elt (elt " + evidenceName + " i) 0) stepsize )) vars-per-slice))) \n" +
                                "( elt (elt " + evidenceName + " i) 1)\n" +
                                ")))";
            return command;
        }
        public String FillContinousEvidence(InputNode node, int timestep)
        {
              String nodename = Subscript(node.toString(), 0);
            String evidenceName = node.GetEvidenceName();
            //String tmstp = String.valueOf(setting);
            String command = "(loop FOR i FROM 0 TO (- (length " + evidenceName + ") 1) DO\n" +
                            "(loop FOR j FROM  (truncate (/ (elt (elt " + evidenceName + " i) 0) stepsize)) TO (1- no-of-slices) DO\n" +
                            "(setf (aref evidence (+ (bnode-index (bnode-by-name " + nodename+ " " + dbn_name + ")) \n" +
                            "(* j vars-per-slice)\n" +
                            ")) \n" +
                            "(elt (elt " + evidenceName + " i) 1) ) \n" +
                            "\n" +
                            ") \n" +
                            ")";
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
        
        public String RunParticleFilter(int samples)
        {
            //(output (particle-filter-by-name nodelist e1 insulin-dbn  :N no-of-samples ;;e1 = output
                                                  
                                                  //))
            String command = "(setf output (particle-filter-by-name nodelist evidence " + 
                    dbn_name + " :N " + String.valueOf(samples) + "))";
            return command;
        }
        public String GetOutputFileName(int samples)
        {
            String path = System.getProperty("user.dir");
            path = path.replace("\\", "/");
            path = path + "/Results/Outputs/";
            
            String name = path  + dbn_name + "_" + String.valueOf(samples)
                    + "_"  + "Fixed" + ".csv";
            return name;
            
        }
        public String WriteOutputFile(int samples)
        {
            String command = "(with-open-file (s \"";
            command = command + GetOutputFileName(samples);
            command = command + "\" :direction :output :if-does-not-exist :create :if-exists :supersede)";
            command = command + "(format s \"Time ~{~S std ~}\" nodelist)\n" +
"                            (fresh-line s)\n";
            command = command + " (LOOP FOR i FROM 0 TO (1- (length (third output))) DO \n" +
"                                  (prin1 (* i stepsize) s) \n" +
"                                  (princ \" \" s)\n" +
"                                  (LOOP FOR j FROM 0 TO (1- (length nodelist)) DO\n" +
                                        
"                                        (format s \"~12,7F\" (first (elt (first (first (subseq (third output) i (+ 1 i)))) j)) )\n" +
"                                        (princ \" \" s)\n" +
"                                        (format s \"~12,7F\" (second (elt (first (first (subseq (third output) i (+ 1 i)))) j)) )\n" +
"                                        (princ \" \" s)\n" +
"                                        )\n" +
                                  
"                                  (fresh-line s)\n" +
"                                  ))";
           return command;
          
        }
        
        
        
     }
    
}
