 /*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Nodes;
import EquationEditor.Tree.*;
import javax.swing.JOptionPane;
import java.util.HashSet;

/**
 *
 * @author Administrator
 */
public class EquationInfo implements java.io.Serializable {
    private String preOrder = "";
    private MathObject expressionTree = null;
    private String equationName = "";
    private String equationStr = "";
    private String equationRHS = "";
    private  HashSet<String> TimeDependentVariables = new HashSet<>(); ;//= new ArrayList<String>(); 
    private  HashSet<String> TimeIndependentVariablesAndConstants = new HashSet<>(); ;
    String errorString = "";
    private String FinalLispExpression = "";
   // private InputComponent[]  inputComponents;
    
//    public EquationInfo(MathObject eq, String eqStr, InputComponent[] inputComponents)
//    {
//        TimeDependentVariables = new HashSet<>();
//        TimeIndependentVariablesAndConstants =  new HashSet<>();
//        expressionTree = eq;
//        equationStr = eqStr; //Temp
//        equationName = eqStr;
//        equationRHS = eqStr;
//        this.inputComponents = inputComponents;
//        ParseEquationTree();
////        System.out.println("HERE");
////        System.out.println(equationName);
////        System.out.println(TimeDependentVariables);
////        System.out.println(TimeIndependentVariablesAndConstants);
//        /*
//        boolean isValid = ParseEquation();
//        if (!isValid)
//        {
//            throw new IllegalArgumentException(errorString);
//        }
//        //FindConflicts();
//        FinalLispExpression = RemoveFunctionOfT(equationRHS);
//        System.out.println("LISP " + FinalLispExpression);*/
//        
//    }
    public EquationInfo()
    {
        
    }
    public void SetName(String name)
    {
        equationName = name.toUpperCase();
    }
   public void SetExpressionTree(MathObject expTree)
   {
       expressionTree = expTree;
   }
    public String GetPreFix()
    {
        //TODO: IMPLEMENT IMPLEMENT IMPLEMENT 
        return equationRHS;
    }
   public void SetPreFix(String rhs)
   {
       equationRHS = rhs;
   }
    public void close() {
        JOptionPane.showMessageDialog(null, "CLSONF");
       // System.out.println("Closing!");
    }
//    public static void main(String args[]) {
//     EquationInfo eq = new EquationInfo(null, "");
//    }
    public HashSet<String> GetTimeDependentVariables()
    {
        return TimeDependentVariables;
    }
    public HashSet<String> GetTimeIndependentVariables()
    {
        return TimeIndependentVariablesAndConstants;
    }
    public void AddTimeDependentVariableToHashSet(String var)
    {
       // var = RemoveFunctionOfT(var);
       // if (!equationName.equals(var))
       // {
        //Important change, if RHS contains the LHS term itself, let it add.
            TimeDependentVariables.add(var.trim().toUpperCase());
       // }
    }
    public String GetName()
    {
        return equationName;
    }
   
     public void AddTimeIndependentVariableToHashSet(String var)
    {        
        TimeIndependentVariablesAndConstants.add(var.trim());
    }
   
//    public void ParseEquationTree()
//    {
//        MathObject start = expressionTree;
//        if (start != null)
//        {
//           if (start.getClass().getName().equals("EquationEditor.Tree.NaryOperator")) 
//           {
//               NaryOperator naryObj = (NaryOperator)start;
//               String equal = inputComponents[naryObj.getID()].getDisplayText();
//               if (equal.equals("="))
//               {
//                   int i = naryObj.getSize();
//                   if (i == 2)
//                   {
//                       ParseEquationLHS(naryObj.getChild(1));
//                       ParseEquationRHS(naryObj.getChild(0));
//                   }
//                   else
//                   {
//                       //eq = null;//throw error
//                   }
//               }
//               else
//               {
//                   //eq = null;//throw error
//               }
//           }
//        }
//        //return eq;
//    }
//    public void ParseEquationLHS(MathObject start)
//    {
//        if (start.getClass().getName().equals("EquationEditor.Tree.NaryFunction"))
//        {
//            NaryFunction naryObj = (NaryFunction)start;
//            String differential = inputComponents[naryObj.getID()].getDisplayText();
//            if (differential.equals("Differential"))
//            {
//                ParseEquationName(naryObj.getChild(0));
//            }
//            else
//            {
//                //eq = null;
//            }
//            
//        }
//    }
//    
//    private void ParseEquationName(MathObject obj)
//    {
//        if (obj.getClass().getName().equals("EquationEditor.Tree.BinaryOperator")) 
//        {
//            BinaryOperator binaryObj = (BinaryOperator)obj;
//            String name = inputComponents[binaryObj.getID()].getDisplayText();
//            
//            if (name.equals("Function"))
//            {
//                String varName = ExtractName(binaryObj.getLeftChild());
//                SetName(varName);
//            }
//            else if (name.equals("Subscript"))
//            {
//                String varName =  GetNameFromSubscript(binaryObj);
//                SetName(varName);
//            }
//            else
//            {
//                //eq = null;//Throw Error
//            }
//        }
//         else if (obj.getClass().getName().equals("EquationEditor.Tree.Text")) 
//            {
//                Text textObj = (Text)obj;
//                SetName(textObj.getText());
//            }
//            else if (obj.getClass().getName().equals("EquationEditor.Tree.Variable")) 
//            {
//                Variable variableObj = (Variable)obj;
//                SetName(Character.toString(variableObj.getVarName()));
//            }
//    }
//    public void ParseEquationRHS(MathObject start)
//    {
//        traverse(start);
//    }
//    public void traverse(MathObject start)
//    {
//           if (start.getClass().getName().equals("EquationEditor.Tree.Text")) 
//           {
//            Text textObj = (Text)start;
//            AddTimeIndependentVariableToHashSet(textObj.getText());
//            }
//           if (start.getClass().getName().equals("EquationEditor.Tree.Variable")) 
//           {
//            Variable variableObj = (Variable)start;
//            AddTimeIndependentVariableToHashSet(Character.toString(variableObj.getVarName()));
//             //DefaultMutableTreeNode var = new DefaultMutableTreeNode(String.valueOf(variableObj.getVarName()));
//            //currentNode.add(var);
//            }
//           if (start.getClass().getName().equals("EquationEditor.Tree.BinaryOperator")) 
//           {
//            BinaryOperator binaryObj = (BinaryOperator)start;
//            
//            String name = inputComponents[binaryObj.getID()].getDisplayText();
//            //DefaultMutableTreeNode binary = new DefaultMutableTreeNode(inputComponents[binaryObj.getID()].getDisplayText());
//            //currentNode.add(binary);
//            if (name.equals("Subscript"))
//            {
//                String varName = GetNameFromSubscript(binaryObj);
//               // String varName = ExtractName(binaryObj.getLeftChild());
//               // varName = varName + "_" + ExtractName(binaryObj.getRightChild());
//                AddTimeIndependentVariableToHashSet(varName);
//            }
//            else if (name.equals("Function"))
//            {
//                String varName = ExtractTimeDependentVarName(binaryObj);
//                AddTimeDependentVariableToHashSet(varName);
//                
//            }
//            else
//            {
//            traverse(binaryObj.getRightChild());
//            traverse(binaryObj.getLeftChild());
//            }
//        }
//        if (start.getClass().getName().equals("EquationEditor.Tree.Function")) 
//        {
//            Function functionObj = (Function)start;
//            traverse(functionObj.getChild());
//            
//        }
//        if (start.getClass().getName().equals("EquationEditor.Tree.Grouping")) {
//            Grouping groupingObj = (Grouping)start;
//            traverse(groupingObj.getChild());
//            
//        }
//        if (start.getClass().getName().equals("EquationEditor.Tree.NaryOperator")) {
//            NaryOperator naryObj = (NaryOperator)start;
//            int i=0;
//            while (i < naryObj.getSize()) {
//                traverse(naryObj.getChild(i));
//                i++;
//            }
//        }
//    }
//    public String ExtractTimeDependentVarName(MathObject obj)
//    {
//        String varName = "";
//        BinaryOperator binaryObj = (BinaryOperator)obj;
//        if (binaryObj.getLeftChild().getClass().getName().equals("EquationEditor.Tree.BinaryOperator"))
//        {
//             String name = inputComponents[binaryObj.getLeftChild().getID()].getDisplayText();
//            
//            if (name.equals("Subscript"))
//            {
//                varName = GetNameFromSubscript(binaryObj.getLeftChild());
//            }
//        }
//       else
//        {
//            varName = ExtractName(binaryObj.getLeftChild());            
//        }
//        return varName;
//    }
//    private String GetNameFromSubscript(MathObject obj)
//    {
//        String varName, leftName, rightName;
//        BinaryOperator binaryObj = (BinaryOperator)obj;
//        leftName = ExtractName(binaryObj.getLeftChild());
//        rightName = ExtractName(binaryObj.getRightChild());
//        if (rightName.equals(""))
//        {
//            if(binaryObj.getRightChild().getClass().getName().equals("EquationEditor.Tree.RealNumber"))
//            {
//                 RealNumber numberObj = (RealNumber)binaryObj.getRightChild();
//                 rightName = numberObj.getNumber(false);
//            }
//        }
//        varName = leftName + "_" + rightName;
//        return varName;
//    }
//    public String ExtractName(MathObject obj)
//    {
//        String name = "";
//        if (obj.getClass().getName().equals("EquationEditor.Tree.Variable"))
//        {
//            Variable variableObj = (Variable)obj;
//            name = Character.toString(variableObj.getVarName());
//        }
//        else if (obj.getClass().getName().equals("EquationEditor.Tree.Text")) 
//        {
//           Text textObj = (Text)obj;
//           name = textObj.getText();
//            //eq.AddTimeIndependentVariableToHashSet(textObj.getText());
//        }
//    return name;   
//    }
    public MathObject GetExpressionTree()
    {
        return this.expressionTree;
    }
    @Override
    public String toString()
    {
        return this.equationName.toUpperCase();
    }
    
   
    
}
