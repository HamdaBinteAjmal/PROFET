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
    