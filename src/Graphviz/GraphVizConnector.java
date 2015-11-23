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
package Graphviz;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.logging.Level;
import java.util.logging.Logger;


public class GraphVizConnector
{
  
   private static String TEMP_DIR = "";	//Store temp files
 
   private static String DOT = "";	

   //The source code of the graph written in dot language, obtained from the 
    //.dot file
	private StringBuilder graph = new StringBuilder();

   
   public GraphVizConnector() {
       TEMP_DIR = System.getProperty("user.dir");
       // 
        TEMP_DIR = TEMP_DIR.replace("\\", "/");
        TEMP_DIR = TEMP_DIR + "/Results/Temp";
        String path = FindSerializedPath();
        if (!path.equals(""))
        {
            DOT = path;
        }
        else
        {
            SetPath();
        }
   }
   //Sets the path of the GraphViz exe file 
   private void SetPath()
   {
       
       if(OSValidator.isWindows())
       {
           DOT = "C:\\Program Files (x86)\\Graphviz2.38\\bin\\dot.exe";
       }
       else if (OSValidator.isUnix())
       {
           DOT = "/usr/bin/dot";
       }
       else if (OSValidator.isMac())
       {
           DOT = "/usr/local/graphviz-2.38/bin/dot";
       }
   }
   
   //Returns the dot language graph source in String form
   public String getDotSource() {
      return graph.toString();
   }

  
   
   //Converts graph to binary image
   public byte[] ConvertGraphToBinImage(String dot_source, String type)
   {
      File dot;
      byte[] img_stream = null;
   
      try {
         dot = writeDotSourceToFile(dot_source);
         if (dot != null)
         {
            img_stream = GetImageStreamFromDot(dot, type);
            
            if (dot.delete() == false) 
               System.err.println(dot.getAbsolutePath() + " could not be deleted!");
            return img_stream;
         }
         return null;
      } catch (java.io.IOException ioe) { return null; }
   }
 

   //Writes tthe binary image byte array to an image file
   public int writeGraphToFile(byte[] img, File to)
   {
      try {
         FileOutputStream fos = new FileOutputStream(to);
         fos.write(img);
         fos.close();
      } catch (java.io.IOException ioe) { return -1; }
      return 1;
   }

   //Call external GraphViz process to convert the dot file to an image
   private byte[] GetImageStreamFromDot(File dot, String type)
   {
      File img;
      byte[] img_stream = null;

      try {
         img = File.createTempFile("graph_", "."+type, new File(GraphVizConnector.TEMP_DIR));
         Runtime rt = Runtime.getRuntime();
         
         // patch by Mike Chenault
         
         String[] args = {DOT, "-T"+type, dot.getAbsolutePath(), "-o", img.getAbsolutePath()};
         Process p = rt.exec(args);
         
         p.waitFor();

         
         FileInputStream in = new FileInputStream(img.getAbsolutePath());
         img_stream = new byte[in.available()];
         in.read(img_stream);
         // Close it if we need to
         if( in != null ) in.close();

         if (img.delete() == false) 
            System.err.println(img.getAbsolutePath() + " could not be deleted!");
      }
      catch (java.io.IOException ioe) {
         System.err.println("Error:    in I/O processing of tempfile in dir " + GraphVizConnector.TEMP_DIR+"\n");
         System.err.println("       or in calling external command");
         ioe.printStackTrace();
      }
      catch (java.lang.InterruptedException ie) {
         System.err.println("Error: the execution of the external program was interrupted");
         ie.printStackTrace();
      }

      return img_stream;
   }

   //Returns a file that holds the dot source of graph written in dot language
   private File writeDotSourceToFile(String str) throws java.io.IOException
   {
      File temp;
      try {
         temp = File.createTempFile("graph_", ".dot.tmp", new File(GraphVizConnector.TEMP_DIR));
         FileWriter fout = new FileWriter(temp);
         fout.write(str);
         fout.close();
      }
      catch (Exception e) {
         System.err.println("Error: I/O error while writing the dot source to temp file!");
         return null;
      }
      return temp;
   }

  
  //Reads a dot source of graph from text file
   public void readSource(String input)
   {
	   StringBuilder sb = new StringBuilder();
	   
	   try
	   {
		   FileInputStream fis = new FileInputStream(input);
		   DataInputStream dis = new DataInputStream(fis);
		   BufferedReader br = new BufferedReader(new InputStreamReader(dis));
		   String line;
		   while ((line = br.readLine()) != null) {
			   sb.append(line);
		   }
		   dis.close();
	   } 
	   catch (Exception e) {
		   System.err.println("Error: " + e.getMessage());
	   }
	   
	   this.graph = sb;
   }
   
   //Get address folder path
   private static String GetDotAddressFolderPath()
   {
        String dotAddress =  System.getProperty("user.dir");
       dotAddress = dotAddress.replace("\\", "/");
      
       dotAddress = dotAddress + "/Results/Graphviz/Address.txt";
       return dotAddress;
   }
   public static void ResetDOTInstallationPath(String newPath)
   {
       
       DOT = newPath; //ALSO SERIALIZE FOR FUTURE TEMP_DIR = System.getProperty("user.dir");
       // 
       SerializePath(newPath);       
      

   }
   private static String FindSerializedPath()
   {
       File file = new File(GetDotAddressFolderPath());
       String path = "";
       if (file.exists()) 
       {
       
           try {
               BufferedReader br = new BufferedReader(new FileReader(file));
               path = br.readLine();
               
           } catch (FileNotFoundException ex) {
               Logger.getLogger(GraphVizConnector.class.getName()).log(Level.SEVERE, null, ex);
           } catch (IOException ex) {
               Logger.getLogger(GraphVizConnector.class.getName()).log(Level.SEVERE, null, ex);
           }
           
       }
       return path;    
       
   }
   private static void SerializePath(String newPath)
   {
        File file = new File(GetDotAddressFolderPath());
       if (!file.exists()) 
       {
           try 
           {
               file.getParentFile().mkdirs();
               file.createNewFile();
           }
           catch (IOException ex)
           {
               Logger.getLogger(GraphVizConnector.class.getName()).log(Level.SEVERE, null, ex);
           }
       }
       FileWriter fw;
       try 
       {
           fw = new FileWriter(file.getAbsoluteFile());      
           BufferedWriter bw = new BufferedWriter(fw);
           bw.write(newPath);
           bw.close();
                         
       } 
       catch (IOException ex) 
       {
           Logger.getLogger(GraphVizConnector.class.getName()).log(Level.SEVERE, null, ex);
       }
  
   }
} // end of class GraphVizConnector

