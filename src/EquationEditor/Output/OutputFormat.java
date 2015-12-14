/* 
 * Copyright (C) 2010 Alex Billingsley, email@alexbillingsley.co.uk
 * www.dragmath.bham.ac.uk 
 * This file is part of DragMath.
 *
 * This file has been modified by Hamda Binte Ajmal to be added
 * to the project PROFET.
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



 
 

package EquationEditor.Output;

import EquationEditor.Tree.BinaryOperator;
import EquationEditor.Tree.NaryOperator;
import EquationEditor.Tree.Grouping;
import EquationEditor.Tree.Precedence;
import EquationEditor.Tree.Function;
import EquationEditor.Tree.MathObject;
import EquationEditor.Tree.NaryFunction;
import EquationEditor.Tree.Matrix;

import org.jdom.*;
import org.jdom.input.*;
import java.net.URL;
import java.io.*;
import javax.swing.*;
import EquationEditor.Display.StatusBar;
import EquationEditor.Display.LanguageManager;
import EquationEditor.Tree.RealNumber;
import EquationEditor.Tree.Variable;
import Nodes.EquationInfo;

/**
 * Class to use configuration files to convert expression to different syntax
 * @author Alex Billingsley
 */
public class OutputFormat {
    
    private String output="";
    private boolean autoBrackets;
    private boolean implicitMult;
    private boolean keepAsDouble;
  
    private Document formatDoc;
    private Element root;
    private Element bracket;
    private SAXBuilder builder;
    
    private String outputFormat;
    
    private URL appletCodeBase;
    private StatusBar status;
    
    private LanguageManager langMan;
    private EquationInfo eq;
    private Boolean isLisp;
    
    /** Creates a new instance of OutputAlgorithm
     * Reads in config. files bundled into JAR file of applet
     */
    public OutputFormat(StatusBar status, LanguageManager langMan, URL appletCodeBase, boolean implicitMult, boolean keepAsDouble) {
        this.status=status;
        autoBrackets=false;
        builder = new SAXBuilder();
        this.appletCodeBase=appletCodeBase;
        this.langMan=langMan;
        this.implicitMult = implicitMult;
        this.keepAsDouble = keepAsDouble;
        this.isLisp = false;
    }
    
    public void setImplictMult(boolean _implicitMult) {
        implicitMult = _implicitMult;
    }
    
    public String getOutputFormat() {
        return outputFormat;
    }
    
    /** Converts expression to format and copies the string to the system clipboard
     */
    public String GetPreFixString(MathObject startNode) {
        autoBrackets=false;
        String syntax="";;
        output = "";
        try {
            root = formatDoc.getRootElement();
            
            bracket = root.getChild("BracketsRnd");
            
            if (startNode != null) {
                Element initial = root.getChild("initial");
                output = output + initial.getChild("output1").getText();
                if (root.getChild("name").getAttributeValue("AutoBrackets").equals("true")) {
                    autoBrackets=true;
                }
                if (root.getChild("name").getValue().equals("Lisp")){
                    isLisp = true;
                }
                convert(startNode);
                output = output + initial.getChild("output2").getText();
                JTextField temp = new JTextField(output);
                syntax = output;
                temp.selectAll();
                //temp.copy(); //Hamda!
                output = "";
                status.println(langMan.readLangFile("Clipboard"));
            } else {
                status.println(langMan.readLangFile("NoExpression"));
            }
            
        } catch (java.lang.NullPointerException err) {
            JOptionPane.showMessageDialog(null, langMan.readLangFile("ReadingFile"), "DragMath", JOptionPane.ERROR_MESSAGE);
        }
        return syntax;
    }
    
    /** Reads in a particular format file specified in the parameters
     */
    public void readFormatFile(String fileName) {
        try {
            
            URL path = LanguageManager.class.getResource("formats/lisp" + ".xml");
            formatDoc = builder.build(path);
            outputFormat = fileName;
        } catch (java.io.FileNotFoundException ex) {
            JOptionPane.showMessageDialog(null, langMan.readLangFile("ReadingFile2") + " " + fileName, "DragMath", JOptionPane.ERROR_MESSAGE);
        } catch (JDOMException ex) {
            JOptionPane.showMessageDialog(null, langMan.readLangFile("ReadingFile2") + " " + fileName, "DragMath", JOptionPane.ERROR_MESSAGE);
        } catch (IOException ex) {
            JOptionPane.showMessageDialog(null, langMan.readLangFile("ReadingFile2") + " " + fileName, "DragMath", JOptionPane.ERROR_MESSAGE);
        }
    }
    
    public void readMathTranFile() {
        try {
            formatDoc = builder.build(this.getClass().getResourceAsStream("MathTran.xml"));
        } catch (java.io.FileNotFoundException ex) {
            JOptionPane.showMessageDialog(null, langMan.readLangFile("ReadingFile2") + "MathTran", "DragMath", JOptionPane.ERROR_MESSAGE);
        } catch (JDOMException ex) {
            JOptionPane.showMessageDialog(null, langMan.readLangFile("ReadingFile2") + "MathTran", "DragMath", JOptionPane.ERROR_MESSAGE);
        } catch (IOException ex) {
            JOptionPane.showMessageDialog(null, langMan.readLangFile("ReadingFile2") + "MathTran", "DragMath", JOptionPane.ERROR_MESSAGE);
        }
    }
    
    /** Traverses the tree left-to-right to build up the expression using data from the format file
     * as the syntax for each component
     */
    public void convert(MathObject start) throws java.lang.NullPointerException {
        //System.out.print(start.getName());
        if (start.getClass().getName().equals("EquationEditor.Tree.Text")) {
            EquationEditor.Tree.Text textObj = (EquationEditor.Tree.Text)start;
            
            if (textObj.getText().equals(("Infinity"))) {
                Element var = root.getChild("Infinity");
                output = output + var.getChild("output").getText();
            } else {
                
                Element text = root.getChild("Text");
                try {
                    output = output + text.getChild("initial").getText();
                } catch (java.lang.NullPointerException err) {
                    // do nothing, as this tag is optional in file
                }
                String temp = textObj.getText();
                if(isLisp){
                  temp   = RemoveCommasForLisp(temp);
                }
                output = output + text.getChild("output1").getText() + temp + text.getChild("output2").getText();
                
            }
        }
        
        if (start.getClass().getName().equals("EquationEditor.Tree.RealNumber")) {
            EquationEditor.Tree.RealNumber numberObj = (EquationEditor.Tree.RealNumber)start;
            Element number = root.getChild("RealNumber");
            try {
                output = output + number.getChild("initial").getText();
            } catch (java.lang.NullPointerException err) {
                // do nothing, as this tag is optional in file
            }
            output = output + number.getChild("output1").getText() + numberObj.getNumber(keepAsDouble) + number.getChild("output2").getText();
        }
        
        if (start.getClass().getName().equals("EquationEditor.Tree.Variable")) {
            EquationEditor.Tree.Variable variableObj = (EquationEditor.Tree.Variable)start;
            Element var;
            
            try {
                // If there is entry in output file containing symbol e.g. greek letter
                var = root.getChild(String.valueOf(variableObj.getVarName()));
                output = output + var.getChild("output").getText();
            } catch (java.lang.NullPointerException err) {
                var = root.getChild(String.valueOf(variableObj.getName()));
                if (variableObj.getName() == "Variable") {
                    output = output + var.getChild("output1").getText();
                    output = output + variableObj.getVarName();
                    output = output + var.getChild("output2").getText();
                } else {
                    output = output + var.getChild("output").getText();
                }
            }
        }
        
        if (start.getClass().getName().equals("EquationEditor.Tree.BinaryOperator")) {
            BinaryOperator binaryObj = (BinaryOperator)start;
            Element binary = root.getChild(binaryObj.getName());
            
            try {
                output = output + binary.getChild("initial").getText();
            } catch (java.lang.NullPointerException err) {
                // do nothing, as this tag is optional in file
            }
            
            boolean brackets = false;
            
            // Checks if user wants auto bracketing
            if (autoBrackets) {
                // Check if brackets are turned on/off in xml
                try {
                    if (binary.getAttributeValue("brackets").equals("true")) brackets=true;
                } catch (java.lang.NullPointerException err)  {
                    // If bracket option is not specified, then default is to include brackets
                    brackets=true;
                }
            }
            // Decide if brackets are required using precedence table and location in the tree
            if (brackets) {
                brackets=false;
                if (binaryObj.getParent() != null) {
                    // If precedence is lower
                    if (Precedence.value[binaryObj.getID()] <= Precedence.value[binaryObj.getParent().getID()]) {
                        brackets=true;
                    }
                }
            }
            
            if (brackets) {
                output = output + bracket.getChild("output1").getText();
            }
            
               output = output + binary.getChild("output1").getText();
           
            
            // If reverse='true' then swap right and left child
            try {
                if (binary.getAttributeValue("reverse").equals("true")) {
                    convert(binaryObj.getRightChild());
                } else {
                    convert(binaryObj.getLeftChild());
                }
            } catch (NullPointerException ex) {
                // default is set to false
                convert(binaryObj.getLeftChild());
            }
            
             output = output.trim() + binary.getChild("output2").getText();
            
            // If reverse='true' then swap right and left child
            try {
                if (binary.getAttributeValue("reverse").equals("true")) {
                    convert(binaryObj.getLeftChild());
                } else {
                    convert(binaryObj.getRightChild());
                }
            } catch (NullPointerException ex) {
                // default is set to false
                if (!binary.getName().equals("Function"))
                convert(binaryObj.getRightChild());
            }
            output = output + binary.getChild("output3").getText().trim();
            if (brackets) {
                output = output + bracket.getChild("output2").getText();
            }
             try {
                output = output + binary.getChild("final").getText();
            } catch (java.lang.NullPointerException err) {
                // do nothing, as this tag is optional in file
            }
            
        }
        
        if (start.getClass().getName().equals("EquationEditor.Tree.Function")) {
            Function functionObj = (Function)start;
            Element function = root.getChild(functionObj.getName());
            
            try {
                output = output + function.getChild("initial").getText();
            } catch (java.lang.NullPointerException err) {
                // do nothing, as this tag is optional in file
            }
            
            output = output + function.getChild("output1").getText();
            
            try {
                if (function.getAttributeValue("brackets").equals("true")) {
                    output = output + bracket.getChild("output1").getText();
                }
            } catch (java.lang.NullPointerException err)  {
                // If bracket option is not specified, then default is to include brackets
                output = output + bracket.getChild("output1").getText();
            }
            
            convert(functionObj.getChild());
            output = output + function.getChild("output2").getText();
            
            try {
                if (function.getAttributeValue("brackets").equals("true")) {
                    output = output + bracket.getChild("output2").getText();
                }
            } catch (java.lang.NullPointerException err)  {
                // If bracket option is not specified, then default is to include brackets
                output = output + bracket.getChild("output2").getText();
            }
             try {
                output = output + function.getChild("final").getText();
            } catch (java.lang.NullPointerException err) {
                // do nothing, as this tag is optional in file
            }
        }
        
        if (start.getClass().getName().equals("EquationEditor.Tree.Matrix")) {
            Matrix matrixObj = (Matrix)start;
            Element matrix = root.getChild(matrixObj.getName());
            int matrix_m = matrixObj.getM();
            int matrix_n = matrixObj.getN();
            
            output = output + matrix.getChild("matrixStart").getText();
            
            int x=0; int y=0;
            while (y < matrix_m) {
                output = output + matrix.getChild("rowStart").getText();
                while (x < matrix_n) {
                    output = output + matrix.getChild("elementStart").getText();
                    convert(matrixObj.getElement(y,x));
                    output = output + matrix.getChild("elementEnd").getText();
                    if (x != matrix_n-1) {
                        output = output + matrix.getChild("elementSeparator").getText();
                    }
                    x++;
                }
                output = output + matrix.getChild("rowEnd").getText();
                if (y != matrix_m-1) {
                    output = output + matrix.getChild("rowSeparator").getText();
                }
                x=0;
                y++;
            }
            output = output + matrix.getChild("matrixEnd").getText();
        }
        
        if (start.getClass().getName().equals("EquationEditor.Tree.Grouping")) {
            Grouping groupingObj = (Grouping)start;
            Element grouping = root.getChild(groupingObj.getName());
            
            try {
                output = output + grouping.getChild("initial").getText();
            } catch (java.lang.NullPointerException err) {
                // do nothing, as this tag is optional in file
            }
            if(!isLisp || grouping.getName().equals("Abs"))
            {
                output = output + grouping.getChild("output1").getText();
            }
            convert(groupingObj.getChild());            
            if(!isLisp || grouping.getName().equals("Abs"))
            {
                output = output + grouping.getChild("output2").getText();
            }
            
            try {
                output = output + grouping.getChild("final").getText();
            } catch (java.lang.NullPointerException err) {
                // do nothing, as this tag is optional in file
            }
            
        }
        
        if (start.getClass().getName().equals("EquationEditor.Tree.NaryOperator")) {
            NaryOperator naryObj = (NaryOperator)start;
            Element nary = root.getChild(naryObj.getName());
            
            try {
                output = output + nary.getChild("initial").getText();
            } catch (java.lang.NullPointerException err) {
                // do nothing, as this tag is optional in file
            }
            
            boolean brackets = false;
            
            // Checks if user wants auto bracketing
            if (autoBrackets) {
                // Check if brackets are turned on/off in xml
                try {
                    if (nary.getAttributeValue("brackets").equals("true")) brackets=true;
                } catch (java.lang.NullPointerException err)  {
                    // If bracket option is not specified, then default is to include brackets
                    brackets=true;
                }
            }
            // Decide if brackets are required using precedence table and location in the tree
            if (brackets) {
                brackets=false;
                if (naryObj.getParent() != null) {
                    // Precedence is lower
                    if (Precedence.value[naryObj.getID()] <= Precedence.value[naryObj.getParent().getID()]) {
                        brackets=true;
                    }
                }
            }
            
            if (brackets) {
                output = output + bracket.getChild("output1").getText();
            }           
             
            output = output + nary.getChild("output").getText();
           
             
            int i = naryObj.getSize()-1;
            while (i >= 1) {
                convert(naryObj.getChild(i));
                
                // Code added to make multiplication implicit if user chooses so
                // If implict mult turned on and operator is multiplication then do not output it's display
                if ((implicitMult && naryObj.getID() == 0) || isLisp ) {
                    // do nothing - as above
                } else {
                    output = output + nary.getChild("output").getText();
                }
                i--;
            }
            convert(naryObj.getChild(i));
            if (brackets) {
                output = output + bracket.getChild("output2").getText();
            }
                        
            try {
                output = output + nary.getChild("final").getText();
            } catch (java.lang.NullPointerException err) {
                // do nothing, as this tag is optional in file
            }
        }
        
        if (start.getClass().getName().equals("EquationEditor.Tree.NaryFunction")) {
            NaryFunction naryFunctionObj = (NaryFunction)start;
            Element naryFunction = root.getChild(naryFunctionObj.getName());
            
            try {
                output = output + naryFunction.getChild("initial").getText();
            } catch (java.lang.NullPointerException err) {
                // do nothing, as this tag is optional in file
            }
            
            output = output + naryFunction.getChild("output1").getText();
            
            String[] orders = null;
            String order = null;
            try {
                order = naryFunction.getAttributeValue("order");
                if (order != null) {
                    orders = order.split(",");
                }
            } catch (java.lang.NullPointerException err)  {
                // do nothing, as this tag is optional in file
            }
            
            int i=0;
            if (order != null) {
                while (i < naryFunctionObj.getSize()) {
                    convert(naryFunctionObj.getChild(Integer.parseInt(orders[i])));
                    int x = i+2;
                    output = output + naryFunction.getChild("output" + String.valueOf(x)).getText();
                    i++;
                }
            } else {
                while (i < naryFunctionObj.getSize()) {
                    convert(naryFunctionObj.getChild(i));
                    int x = i+2;
                    output = output + naryFunction.getChild("output" + String.valueOf(x)).getText();
                    i++;
                }
            }
            try {
                output = output + naryFunction.getChild("final").getText();
            } catch (java.lang.NullPointerException err) {
                // do nothing, as this tag is optional in file
            }
        }
    }
    public String RemoveCommasForLisp(String output)
    {
        String noCommas = output.replaceAll(",", " ");
        return noCommas;
    }
    
    ////////////////////////////////////////////////////////////////////////////
    private MathObject DivideRLHS(MathObject tree)
    {
        MathObject RHS = null;
        if (tree != null)
        {
           if (tree.getClass().getName().equals("EquationEditor.Tree.NaryOperator")) 
           {
               NaryOperator naryObj = (NaryOperator)tree;
               String equal = naryObj.getName();//inputComponents[naryObj.getID()].getDisplayText();
               if (equal.equals("Equals"))
               {
                   int i = naryObj.getSize();
                   if (i == 2)
                   {
                       RHS = naryObj.getChild(0);
                   }
               }
           }
        }
        return RHS;
    }
     public EquationInfo ParseEquationTree(MathObject expressionTree)
    {
        eq = new EquationInfo();
        MathObject RHS = DivideRLHS(expressionTree);
        String preFix = GetPreFixString(RHS);
       // JOptionPane.showMessageDialog(null, preFix);
        eq.SetPreFix(preFix);
        
        MathObject start = expressionTree;
        eq.SetExpressionTree( expressionTree);
        
        if (start != null)
        {
           if (start.getClass().getName().equals("EquationEditor.Tree.NaryOperator")) 
           {
               NaryOperator naryObj = (NaryOperator)start;
               String equal = naryObj.getName();//inputComponents[naryObj.getID()].getDisplayText();
               if (equal.equals("Equals"))
               {
                   int i = naryObj.getSize();
                   if (i == 2)
                   {
                       eq.SetName(ParseEquationLHS(naryObj.getChild(1)));
                       ParseEquationRHS(naryObj.getChild(0));
                   }
                   else
                   {
                       //eq = null;//throw error
                   }
               }
               else
               {
                   //eq = null;//throw error
               }
           }
        }
        return eq;
    }
    public String ParseEquationLHS(MathObject start)
    {
        String name = "";
        if (start.getClass().getName().equals("EquationEditor.Tree.NaryFunction"))
        {
            NaryFunction naryObj = (NaryFunction)start;
            String differential =  naryObj.getName();//inputComponents[naryObj.getID()].getDisplayText();
            if (differential.equals("Differential"))
            {
                name = ParseEquationName(naryObj.getChild(0));
            }
            else
            {
                //eq = null;
            }
            
        }
        return name;
    }
    
    private String ParseEquationName(MathObject obj)
    {
        String varName = "";
        if (obj.getClass().getName().equals("EquationEditor.Tree.BinaryOperator")) 
        {
            BinaryOperator binaryObj = (BinaryOperator)obj;
            String name = binaryObj.getName();//inputComponents[binaryObj.getID()].getDisplayText();
            
            if (name.equals("Function"))
            {
               // varName = ExtractName(binaryObj.getLeftChild());
                varName = ExtractTimeDependentVarName(binaryObj);
                //SetName(varName);
            }
            else if (name.equals("Subscript"))
            {
                varName =  GetNameFromSubscript(binaryObj);
                //SetName(varName);
            }
            else
            {
                //eq = null;//Throw Error
            }
        }
         else if (obj.getClass().getName().equals("EquationEditor.Tree.Text")) 
            {
                EquationEditor.Tree.Text textObj = (EquationEditor.Tree.Text)obj;
                varName = textObj.getText();
                //SetName(textObj.getText());
            }
            else if (obj.getClass().getName().equals("EquationEditor.Tree.Variable")) 
            {
                Variable variableObj = (Variable)obj;
                Element var;
                try {
                // If there is entry in output file containing symbol e.g. greek letter
                var = root.getChild(String.valueOf(variableObj.getVarName()));
                varName = var.getChild("output").getText();
                }
                catch (java.lang.NullPointerException err) 
                {
                    var = root.getChild(String.valueOf(variableObj.getName()));
                    if (variableObj.getName() == "Variable") 
                    {
                        //varName = output + var.getChild("output1").getText();
                        varName = Character.toString(variableObj.getVarName());
                       // output = output + var.getChild("output2").getText();
                    } else 
                    {
                         varName = var.getChild("output").getText();
                    }
                }
            }
        return varName;
    }
    public void ParseEquationRHS(MathObject start)
    {
        traverse(start);
    }
    public void traverse(MathObject start)
    {
           if (start.getClass().getName().equals("EquationEditor.Tree.Text")) 
           {
            EquationEditor.Tree.Text textObj = (EquationEditor.Tree.Text)start;
            eq.AddTimeIndependentVariableToHashSet(textObj.getText());
            }
           if (start.getClass().getName().equals("EquationEditor.Tree.Variable")) 
           {
            Variable variableObj = (Variable)start;
            String varName = "";
              Element var;
                try {
                // If there is entry in output file containing symbol e.g. greek letter
                var = root.getChild(String.valueOf(variableObj.getVarName()));
                varName = var.getChild("output").getText();
                }
                catch (java.lang.NullPointerException err) 
                {
                    var = root.getChild(String.valueOf(variableObj.getName()));
                    if (variableObj.getName() == "Variable") 
                    {
                        //here
                        try {
                                // If there is entry in output file containing symbol e.g. greek letter
                                var = root.getChild(String.valueOf(variableObj.getVarName()));
                                varName = var.getChild("output").getText();
                            }
                            catch (java.lang.NullPointerException err1) 
                            {
                                var = root.getChild(String.valueOf(variableObj.getName()));
                                if (variableObj.getName() == "Variable") 
                                {
                                    //varName = output + var.getChild("output1").getText();
                                    varName = Character.toString(variableObj.getVarName());
                                    // output = output + var.getChild("output2").getText();
                                } else 
                                {
                                    varName = var.getChild("output").getText();
                                }
                            }
                        //here
                        //varName = output + var.getChild("output1").getText();
                        //varName = Character.toString(variableObj.getVarName());
                       // output = output + var.getChild("output2").getText();
                    } else 
                    {
                         varName = var.getChild("output").getText();
                    }
                }
            eq.AddTimeIndependentVariableToHashSet(varName);
             //DefaultMutableTreeNode var = new DefaultMutableTreeNode(String.valueOf(variableObj.getVarName()));
            //currentNode.add(var);
            }
           if (start.getClass().getName().equals("EquationEditor.Tree.BinaryOperator")) 
           {
            BinaryOperator binaryObj = (BinaryOperator)start;
            
            String name = binaryObj.getName();//inputComponents[binaryObj.getID()].getDisplayText();
            //DefaultMutableTreeNode binary = new DefaultMutableTreeNode(inputComponents[binaryObj.getID()].getDisplayText());
            //currentNode.add(binary);
            if (name.equals("Subscript"))
            {
                String varName = GetNameFromSubscript(binaryObj);
               // String varName = ExtractName(binaryObj.getLeftChild());
               // varName = varName + "_" + ExtractName(binaryObj.getRightChild());
                eq.AddTimeIndependentVariableToHashSet(varName);
            }
            else if (name.equals("Function"))
            {
                String varName = ExtractTimeDependentVarName(binaryObj);
                eq.AddTimeDependentVariableToHashSet(varName);
                
            }
            else
            {
            traverse(binaryObj.getRightChild());
            traverse(binaryObj.getLeftChild());
            }
        }
        if (start.getClass().getName().equals("EquationEditor.Tree.Function")) 
        {
            Function functionObj = (Function)start;
            traverse(functionObj.getChild());
            
        }
        if (start.getClass().getName().equals("EquationEditor.Tree.Grouping")) {
            Grouping groupingObj = (Grouping)start;
            traverse(groupingObj.getChild());
            
        }
        if (start.getClass().getName().equals("EquationEditor.Tree.NaryOperator")) {
            NaryOperator naryObj = (NaryOperator)start;
            int i=0;
            while (i < naryObj.getSize()) {
                traverse(naryObj.getChild(i));
                i++;
            }
        }
    }
    public String ExtractTimeDependentVarName(MathObject obj)
    {
        //TODO: FIX FIX FIX BUGGGGGGG
        String varName = "";
        BinaryOperator binaryObj = (BinaryOperator)obj;
        if (binaryObj.getLeftChild().getClass().getName().equals("EquationEditor.Tree.BinaryOperator"))
        {
             String name = binaryObj.getLeftChild().getName();//inputComponents[binaryObj.getLeftChild().getID()].getDisplayText();
            
            if (name.equals("Subscript"))
            {
                varName = GetNameFromSubscript(binaryObj.getLeftChild());
            }
            if (name.equals("Function"))
            {
                varName = ExtractName(binaryObj.getLeftChild());
            }
            
        }
       else
        {
            varName = ExtractName(binaryObj.getLeftChild());            
        }
        return varName;
    }
    private String GetNameFromSubscript(MathObject obj)
    {
        String varName, leftName, rightName;
        BinaryOperator binaryObj = (BinaryOperator)obj;
        leftName = ExtractName(binaryObj.getLeftChild());
        rightName = ExtractName(binaryObj.getRightChild());
        if (rightName.equals(""))
        {
            if(binaryObj.getRightChild().getClass().getName().equals("EquationEditor.Tree.RealNumber"))
            {
                 RealNumber numberObj = (RealNumber)binaryObj.getRightChild();
                 rightName = numberObj.getNumber(false);
            }
        }
        varName = leftName.trim() + "_" + rightName.trim();
        return varName;
    }
    public String ExtractName(MathObject obj)
    {
        String varName = "";
        if (obj.getClass().getName().equals("EquationEditor.Tree.Variable"))
        {
            Variable variableObj = (Variable)obj;
            Element var;
            //name = Character.toString(variableObj.getVarName());
           /// 
            //here
            try {
                // If there is entry in output file containing symbol e.g. greek letter
                var = root.getChild(String.valueOf(variableObj.getVarName()));
                varName = var.getChild("output").getText();
                }
                catch (java.lang.NullPointerException err) 
                {
                    var = root.getChild(String.valueOf(variableObj.getName()));
                    if (variableObj.getName() == "Variable") 
                    {
                        //here
                        try {
                                // If there is entry in output file containing symbol e.g. greek letter
                                var = root.getChild(String.valueOf(variableObj.getVarName()));
                                varName = var.getChild("output").getText();
                            }
                            catch (java.lang.NullPointerException err1) 
                            {
                                var = root.getChild(String.valueOf(variableObj.getName()));
                                if (variableObj.getName() == "Variable") 
                                {
                                    //varName = output + var.getChild("output1").getText();
                                    varName = Character.toString(variableObj.getVarName());
                                    // output = output + var.getChild("output2").getText();
                                } else 
                                {
                                    varName = var.getChild("output").getText();
                                }
                            }
                        //here
                        //varName = output + var.getChild("output1").getText();
                        //varName = Character.toString(variableObj.getVarName());
                       // output = output + var.getChild("output2").getText();
                    } else 
                    {
                         varName = var.getChild("output").getText();
                    }
                }
            //here
        }
        else if (obj.getClass().getName().equals("EquationEditor.Tree.Text")) 
        {
           EquationEditor.Tree.Text textObj = (EquationEditor.Tree.Text)obj;
           varName = textObj.getText();
            //eq.AddTimeIndependentVariableToHashSet(textObj.getText());
        }
    return varName;   
    }
    }



