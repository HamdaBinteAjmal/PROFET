/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package Lisp;
import java.net.URL;
import org.armedbear.lisp.*;
/**
 *
 * @author Hamda bint e Ajmal
 * This class provides an interface to connect to AIMA framework written in LISP
 * Code
 */
public class LispConnector 
{

    public LispConnector() {
        StaticLispConnector.Setup();
    }
    
    public static class StaticLispConnector 
    {
        private static Interpreter interpreter;
        
        public static Interpreter GetInterpretor()
        {
            if (interpreter == null)
                SetInterpretor();
            return interpreter;
        }
        public static void Setup()
        {
            SetInterpretor();
            Load_Aima();
        }  
        private static String GetFullPath(String relPath)
        {
             URL url= LispConnector.class.getResource(relPath);
             System.out.println(url.toString());
             return url.toString();
          }
        public static LispObject execute(String lispStatement)
        {
            LispObject result = new LispObject();
            System.out.println(lispStatement);
           interpreter.eval(lispStatement);
            return result;
        }
        private static void SetInterpretor()
        {
            interpreter = Interpreter . getInstance ();
            if ( interpreter == null ) 
            {
                interpreter = Interpreter . createInstance ();
                
                SetDebuggerHook();
            }            
        }
        private static void Load_Aima()
        {
            String path = GetFullPath("aima/defpackage.lisp");
            execute("(load " + "\"" + path + "\")");
            execute("(in-package :aima)");
            
            path = GetFullPath("aima/probability/domains/edit-nets.lisp");
            execute("(load " + "\"" + path + "\")");
            
            path = GetFullPath("aima/aima.lisp");
            execute("(load " + "\"" + path + "\")");        
            execute("(aima-load-binary 'all)");
            path = GetFullPath("aima/Functions.lisp");
            execute("(load " + "\"" + path + "\")");  
            path = GetFullPath("aima/probability/algorithms/dbn-adaptive-inference.lisp");
            execute("(load " + "\"" + path + "\")");
            path = GetFullPath("aima/DBNCInterface/run-inference.lisp");
            execute("(load " + "\"" + path + "\")");
        }
        public static void SetDebuggerHook()
        {
            execute("(setf *debugger-hook*  #'sys::%debugger-hook-function)");
        }
   }
        
    }
    