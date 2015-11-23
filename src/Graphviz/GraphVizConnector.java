// GraphVizConnector.java - a simple API to call dot from Java programs

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
   /**
    * The dir. to store temp files
    */
   private static String TEMP_DIR = "/tmp";	// Linux
 
   private static String DOT = "";	

   /**
    * The source of the graph written in dot language.
    */
	private StringBuilder graph = new StringBuilder();

   /**
    * Constructor: creates a new GraphViz object that will contain
    * a graph.
    */
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
   /**
    * Returns the graph's source description in dot language.
    * @return Source of the graph in dot language.
    */
   public String getDotSource() {
      return graph.toString();
   }

   /**
    * Adds a string to the graph's source (without newline).
    */
  
   /**
    * Returns the graph as an image in binary format.
    * @param dot_source Source of the graph to be drawn.
    * @param type Type of the output image to be produced, e.g.: gif, dot, fig, pdf, ps, svg, png.
    * @return A byte array containing the image of the graph.
    */
   public byte[] getGraph(String dot_source, String type)
   {
      File dot;
      byte[] img_stream = null;
   
      try {
         dot = writeDotSourceToFile(dot_source);
         if (dot != null)
         {
            img_stream = get_img_stream(dot, type);
            
            if (dot.delete() == false) 
               System.err.println("Warning: " + dot.getAbsolutePath() + " could not be deleted!");
            return img_stream;
         }
         return null;
      } catch (java.io.IOException ioe) { return null; }
   }
 

   /**
    * Writes the graph's image in a file.
    * @param img   A byte array containing the image of the graph.
    * @param to    A File object to where we want to write.
    * @return Success: 1, Failure: -1
    */
   public int writeGraphToFile(byte[] img, File to)
   {
      try {
         FileOutputStream fos = new FileOutputStream(to);
         fos.write(img);
         fos.close();
      } catch (java.io.IOException ioe) { return -1; }
      return 1;
   }

   /**
    * It will call the external dot program, and return the image in
    * binary format.
    * @param dot Source of the graph (in dot language).
    * @param type Type of the output image to be produced, e.g.: gif, dot, fig, pdf, ps, svg, png.
    * @return The image of the graph in .gif format.
    */
   private byte[] get_img_stream(File dot, String type)
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
            System.err.println("Warning: " + img.getAbsolutePath() + " could not be deleted!");
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

   /**
    * Writes the source of the graph in a file, and returns the written file
    * as a File object.
    * @param str Source of the graph (in dot language).
    * @return The file (as a File object) that contains the source of the graph.
    */
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

  
   /**
    * Read a DOT graph from a text file.
    * 
    * @param input Input text file containing the DOT graph
    * source.
    */
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

