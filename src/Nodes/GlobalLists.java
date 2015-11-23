/* 
 * PROFET Copyright 2015 (c) Data Mining and Machine Learning Group,
 * National University of Ireland Galway.  
 * This file is a part of PROFET  
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package Nodes;

import MainInterface.Tree;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import javax.swing.JOptionPane;

/**
 *
 * @author Administrator
 */
public class GlobalLists {
    
    public static HashSet<EquationInfo> EQUATIONS = new HashSet<EquationInfo>();
    public static HashMap<ModelParameterNode, Integer> ModelParameters = 
            new HashMap<ModelParameterNode, Integer>();
    public static HashMap<ModelVariableNode, Integer> ModelVariables = 
            new HashMap<ModelVariableNode, Integer>();
   public static HashMap<InputNode, Integer> InputNodes = 
           new HashMap<InputNode, Integer>();
   public static HashMap<Constant, Integer> Constants = new HashMap<Constant, Integer>();
   //public static HashMap<String, Integer> ConfusedList = new HashMap<String, Integer>();
   //Tree.TreeStatic tree = new Tree.TreeStatic();

    public static void AddAllNodesToTree() {
      // AddInputNodesToTree();
      // AddModelParametersToTree();
      // AddModelVariablesToTree();
    }
    public static void AddModelParametersToTree()
    {
    
    }

    public static void RemoveEvidenceNode(ModelVariableNode node) {
        String name = "Observed-" + node.toString() ;
        Map.Entry matchedEntry = FindKey(InputNodes, name);
        
            if (matchedEntry != null)
            {
                InputNodes.remove((InputNode)matchedEntry.getKey());
                Tree.TreeStatic.RemoveNodeFromTree(matchedEntry.getKey());
            }
            
        //throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    static void AddEvidenceNode(InputNode evidenceNode) {
        //throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    InputNodes.put(evidenceNode, 1);
    Tree.TreeStatic.AddNodeToTree(evidenceNode);
    }

    public static void RemoveEverything() {
         EQUATIONS.clear();
        ModelParameters.clear();
        ModelVariables.clear();
        InputNodes.clear();
        Constants.clear();
    }

   
    public HashSet<EquationInfo> GetAllEquations()
    {
        return EQUATIONS;
    }
    public static void AddEquation(EquationInfo equation)
    {
        boolean add = true;
        if(EQUATIONS.add(equation))
        {
            UpdateAllLists(equation, add);
        }
        else
        {
            JOptionPane.showMessageDialog(null, "Already Exists");
        }
    }
    public static void DeleteEquation(EquationInfo equation)
    {
       boolean delete = false;
       if (EQUATIONS.remove(equation))
       {
           UpdateAllLists(equation, delete);
       }
       
    }
    private static void UpdateAllLists(EquationInfo equation, boolean add)
    {
        if (add)
        {
            UpdateModelVariablesToAdd(equation);
            UpdateInputNodesToAdd(equation);
            UpdateConstantsToAdd(equation);
            //oiAddAllNodesToTree();
        } 
        else
        {
            UpdateModelVariablesToDelete(equation.GetName());
            UpdateInputNodesToDelete(equation.GetTimeDependentVariables());
            UpdateModelParamtersAndConstantsToDelete(equation.GetTimeIndependentVariables());
            
        }
    }
    private static void UpdateModelVariablesToDelete(String eqModelVariable)
    {
        
       
            Map.Entry matchedEntry = FindKey(ModelVariables, eqModelVariable);
            if (matchedEntry != null)
            {
                if ((Integer)matchedEntry.getValue() > 1)
                {
                    // add to input nodes
                    InputNode node = new InputNode(eqModelVariable);
                    InputNodes.put(node, (Integer)matchedEntry.getValue() - 1);
                    Tree.TreeStatic.AddNodeToTree(node);
                }
                 ModelVariables.remove((ModelVariableNode)matchedEntry.getKey());
                 ModelVariableNode node = (ModelVariableNode)matchedEntry.getKey();
                 if (node.alsoAnEvidenceNode)
                 {
                     RemoveEvidenceNode(node);
                 }
                    // Remove from tree
                 Tree.TreeStatic.RemoveNodeFromTree(matchedEntry.getKey());
            }
       
        
    }
    private static void UpdateInputNodesToDelete(HashSet<String> eqInputNodes)
    {
        for (String s : eqInputNodes)
        {
            Map.Entry matchedEntry = FindKey(ModelVariables, s);
            if (matchedEntry != null) //exitst a model variable of same name, simply decrement the count
            {
                int count = (Integer)matchedEntry.getValue() - 1;
                ModelVariables.put((ModelVariableNode)matchedEntry.getKey(),count);
            }
            else
            {
                matchedEntry = FindKey(InputNodes, s);
                if (matchedEntry != null)
                {
                    if ((Integer)matchedEntry.getValue() > 1)
                    {
                        int count = (Integer)matchedEntry.getValue() - 1;
                        InputNodes.put((InputNode) matchedEntry.getKey(), count );
                    }
                    else
                    {
                        InputNodes.remove((InputNode)matchedEntry.getKey());
                        Tree.TreeStatic.RemoveNodeFromTree((InputNode)matchedEntry.getKey());
                    }
                }
            }
        }
        
    }
    private static void UpdateModelParamtersAndConstantsToDelete(HashSet<String> eqTimeIndTerms)
    {
        for (String s : eqTimeIndTerms)
        {
           
            Map.Entry matchedEntry = FindKey(Constants, s);
            if (matchedEntry != null)
            {
                if ((Integer)matchedEntry.getValue() > 1)
                {
                    int count = (Integer)matchedEntry.getValue() - 1;
                    Constants.put((Constant)matchedEntry.getKey(), count);
                }
                else
                {
                    Constants.remove((Constant)matchedEntry.getKey());
                }
            }
            else
            {
                matchedEntry = FindKey(ModelParameters, s);
                if (matchedEntry != null)
                {
                    if ((Integer)matchedEntry.getValue() > 1 )
                    {
                        int count = (Integer)matchedEntry.getValue()- 1;
                        ModelParameters.put((ModelParameterNode)matchedEntry.getKey(), count);
                    }
                    else
                    {
                        ModelParameters.remove((ModelParameterNode)matchedEntry.getKey());
                        Tree.TreeStatic.RemoveNodeFromTree(matchedEntry.getKey());
                    }
                }
            }
        }
    }
    public static Map.Entry FindKey(HashMap hs, String str)
    {
        Map.Entry returnObj = null;
        Iterator it = hs.entrySet().iterator();
        while (it.hasNext()) 
        {
            Map.Entry pair = (Map.Entry)it.next();
            if(pair.getKey().toString().equals(str))
            {
                returnObj = pair;
            }
        }
        return returnObj;
    }
    
    private static void UpdateModelVariablesToAdd(EquationInfo equation)
    {
        String variable = equation.GetName();
           int count = 1;
           int count1 = 0;
           Map.Entry matchedEntry = FindKey(InputNodes, variable);
           if (matchedEntry != null)  // InputNodes.containsKey(variable))
           {
               count1 = (Integer)matchedEntry.getValue();
               InputNodes.remove((InputNode)matchedEntry.getKey());
               Tree.TreeStatic.RemoveNodeFromTree(matchedEntry.getKey());
               //Remove Input Node from Tree
           }
           
           matchedEntry = FindKey(ModelVariables, variable);
           if(matchedEntry != null)
           {
               ModelVariables.put((ModelVariableNode)matchedEntry.getKey(),
                       (Integer)matchedEntry.getValue()+count);             
               
               
           }
           else
           {
                //Add Model Variable to Tree
               ModelVariableNode node = new ModelVariableNode(variable);
               ModelVariables.put(node, count + count1);
               Tree.TreeStatic.AddNodeToTree(node);
           }
    }
    private static  void UpdateInputNodesToAdd(EquationInfo equation)
    {
       for(String timeDepVar : equation.GetTimeDependentVariables())
            {
               // ModelVariableNode mv = new ModelVariableNode(timeDepVar);
                Map.Entry matchedEntry = FindKey(ModelVariables, timeDepVar);
                
                if(matchedEntry != null)
                {
                   // ModelVariableNode modelVar = 
                            //new ModelVariableNode(matchedEntry.getKey().toString());
                    
                    ModelVariables.put((ModelVariableNode)matchedEntry.getKey(), 
                            (Integer)matchedEntry.getValue() +1);
                }
                else
                {
                    matchedEntry = FindKey(InputNodes, timeDepVar);
                    
                    if(matchedEntry != null)
                    {
                        
                        InputNodes.put((InputNode)matchedEntry.getKey(), 
                                (Integer)matchedEntry.getValue() + 1);
                    }
                    else
                    {
                        InputNode inputNode = new InputNode(timeDepVar);
                        InputNodes.put(inputNode, 1);
                        Tree.TreeStatic.AddNodeToTree(inputNode);
                    }
                }
        }
   
    }
    private static void UpdateModelParamsToAdd(EquationInfo equation)
    {
         for(String timeIndVar : equation.GetTimeIndependentVariables())
            {
               // ModelVariableNode mv = new ModelVariableNode(timeDepVar);
                Map.Entry matchedEntry = FindKey(ModelParameters, timeIndVar);
                
                if(matchedEntry != null)
                {
                     ModelParameters.put((ModelParameterNode)matchedEntry.getKey(), 
                            (Integer)matchedEntry.getValue() +1);
                }
                else //not found in model Parameters
                {
                    Map.Entry matchedEntry1 = FindKey(Constants, timeIndVar);
                    if (matchedEntry1 == null)
                    {
                         Constants.put( new Constant(timeIndVar),1);
                    }
                
                   
                }
            }
        
    }
    private static void UpdateConstantsToAdd(EquationInfo equation)
    {
         for(String timeIndVar : equation.GetTimeIndependentVariables())
            {
               // ModelVariableNode mv = new ModelVariableNode(timeDepVar);
                Map.Entry matchedEntry = FindKey(ModelParameters, timeIndVar);
                
                if(matchedEntry != null)
                {
                     ModelParameters.put((ModelParameterNode)matchedEntry.getKey(), 
                            (Integer)matchedEntry.getValue() +1);
                }
                else //not found in model Parameters
                {
                    Map.Entry matchedEntry1 = FindKey(Constants, timeIndVar);
                    if (matchedEntry1 != null)
                    {
                        Constants.put((Constant)matchedEntry1.getKey(),(Integer) matchedEntry1.getValue()+1);
                    }
                    else
                    {
                         Constants.put( new Constant(timeIndVar),1);
                    }
                
                   
                }
            }
    }
    public static void DisplayAllLists()
    {
        System.out.println(ModelVariables);
        System.out.println(InputNodes);
       // System.out.println(ConfusedList);
        System.out.println(Constants);
        System.out.println(ModelParameters);
    }
    public static void AddConstant(String name)
    {
        Constant newC = new Constant(name);
        Map.Entry matchedEntry = FindKey(ModelParameters, name);
        if (matchedEntry != null)
        {
            ModelParameters.remove(matchedEntry.getKey());
            Tree.TreeStatic.RemoveNodeFromTree(matchedEntry.getKey());
            //Remove from tree too
            
            Constants.put(newC, (Integer)matchedEntry.getValue());
            //Tree.TreeStatic.AddNodeToTree(name);
        }
        else
        {
            //Probably wont get here
              Constants.put(newC, 1);
        }
        
    }
   
    public static void AddInputNodesToTree()
    {
        HashMap<InputNode, Integer> list =  GlobalLists.InputNodes;
        Iterator it = list.entrySet().iterator();
        while (it.hasNext()) 
        {
             Map.Entry pair = (Map.Entry)it.next();
             //Tree.TreeStatic tree = new Tree.TreeStatic();
             Tree.TreeStatic.AddNodeToTree(pair.getKey());           
        }
    }
//    public static void AddModelVariablesToTree()
//    {
//       /* HashMap<ModelVariableNode, Integer> list =  GlobalLists.ModelVariables;
//        Iterator it = list.entrySet().iterator();
//        while (it.hasNext()) 
//        {
//             Map.Entry pair = (Map.Entry)it.next();
//             //Tree.TreeStatic tree = new Tree.TreeStatic();
//             Tree.TreeStatic.AddNodeToTree(pair.getKey());           
//        }*/
//    }
    public static void AddModelParameter(String name)
    {
         //Map.Entry matchedEntryConfusedList = FindKey(ConfusedList, name);
//         if (isEvidenceNode)
//         {
//             name = "Observed-" + name;
//         }
         ModelParameterNode node = new ModelParameterNode(name);
       //node.SetIsEvidenceNode(isEvidenceNode);
          //Constant newC = new Constant(name);
        Map.Entry matchedEntry = FindKey(Constants, name);
        if (matchedEntry != null)
        {
            Constants.remove((Constant)matchedEntry.getKey());
            //Remove from tree too
            Tree.TreeStatic.RemoveNodeFromTree(matchedEntry.getKey());
            ModelParameters.put(node, (Integer)matchedEntry.getValue());
            Tree.TreeStatic.AddNodeToTree(node);
        }
        else
        {
            ModelParameters.put(node, 1);
             Tree.TreeStatic.AddNodeToTree(node);
        }
  }
    
    public void RemoveEvidenceNode(InputNode node)
    {
        InputNodes.remove(node);
        Tree.TreeStatic.RemoveNodeFromTree(node);
    }
    
    public static void LoadProject(HashSet<EquationInfo> equations,
           HashMap<ModelParameterNode, Integer> mp,
           HashMap<ModelVariableNode, Integer> mv,
           HashMap<InputNode, Integer> in,
           HashMap<Constant, Integer> cnsts      
           )
    {
        RemoveEverything();
       EQUATIONS = equations;
       ModelParameters = mp;
       ModelVariables = mv;
       InputNodes = in;
       Constants = cnsts;
       
       for (EquationInfo eq : EQUATIONS)
       {
           Tree.TreeStatic.LoadEquationFromFile(eq);
       }
              
       HashMap<ModelParameterNode, Integer> list =  GlobalLists.ModelParameters;
        Iterator it = list.entrySet().iterator();
        while (it.hasNext()) 
        {
             Map.Entry pair = (Map.Entry)it.next();
             //Tree.TreeStatic tree = new Tree.TreeStatic();
             Tree.TreeStatic.AddNodeToTree(pair.getKey());         
             
             
             
        }  
        
        HashMap<ModelVariableNode, Integer> list1 =  GlobalLists.ModelVariables;
        it = list1.entrySet().iterator();
        while (it.hasNext()) 
        {
             Map.Entry pair = (Map.Entry)it.next();
             //Tree.TreeStatic tree = new Tree.TreeStatic();
             Tree.TreeStatic.AddNodeToTree(pair.getKey());           
        } 
        
        HashMap<InputNode, Integer> list2 =  GlobalLists.InputNodes;
        it = list2.entrySet().iterator();
        while (it.hasNext()) 
        {
             Map.Entry pair = (Map.Entry)it.next();
             //Tree.TreeStatic tree = new Tree.TreeStatic();
             Tree.TreeStatic.AddNodeToTree(pair.getKey());           
        } 
        
       
        
    }
    
    
    
}
