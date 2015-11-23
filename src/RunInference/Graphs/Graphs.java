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
package RunInference.Graphs;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Vector;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.MutableComboBoxModel;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;
import org.math.plot.Plot2DPanel;

/**
 *
 * @author Administrator
 */
public class Graphs extends javax.swing.JPanel {
    public static class GraphsInner extends javax.swing.JPanel implements ActionListener{
      
    /**
     * Creates new form Graphs
     */
        private String filename = "";
        JLabel title = new JLabel("Select the output file you want to plot");
        JButton browse = new JButton("Browse");
        JButton plot = new JButton("Plot Graph");
        private JComboBox fields = new JComboBox();
        private static Plot2DPanel plotPanel;
        private static int trendline;
        String[] header;
        
    public GraphsInner() {
        initComponents();
        header = new String[1];
        BorderLayout layout = new BorderLayout();
        this.setLayout(layout);
        FlowLayout topLayout = new FlowLayout();
        topLayout.setHgap(10);
        topLayout.setAlignment(FlowLayout.LEFT);
        JPanel topPanel = new JPanel();
        topPanel.setLayout(topLayout);
        topPanel.add(title);
        topPanel.add(browse);
        topPanel.add(fields);
        fields.setPrototypeDisplayValue("text here text");
        
        topPanel.add(plot);
        plot.addActionListener(this);
        this.add(topPanel, BorderLayout.PAGE_START);
        plotPanel = new Plot2DPanel();
        this.add(plotPanel, BorderLayout.CENTER);
        browse.addActionListener(this);
        
    }
    
    private String Browse()
    {
        JFileChooser fc = new JFileChooser();
        
        Action details = fc.getActionMap().get("viewTypeDetails");
        details.actionPerformed(null);
        
        FileFilter filter = new FileNameExtensionFilter("csv file","csv");
        String path = System.getProperty("user.dir") + "\\Results\\Outputs";
        fc.setCurrentDirectory(new java.io.File(path));
        fc.setDialogTitle("Select the file you want to plot");
        fc.setFileFilter(filter);
        int returnVal = fc.showOpenDialog(this);
          
        if (returnVal == JFileChooser.APPROVE_OPTION) 
        {
        File file = fc.getSelectedFile();
        filename = file.getAbsolutePath();
        return filename;
        }
        else 
            return "";
        
       
    }
        @Override
    public String toString()
    {
        return "Plot Predicted Data";
    }

    private void ReadCSVHeader()
    {
        String csvFile = filename;
	BufferedReader br = null;
	String line = "";
	String cvsSplitBy = " ";
        
        Vector<String> filteredHeader = new Vector<>();
       
	try {
 
		br = new BufferedReader(new FileReader(csvFile));
		if ((line = br.readLine()) != null) 
                {
 
		        // use comma as separator
			header = line.split(cvsSplitBy);
 
//			System.out.println("Country [code= " + country[4] 
//                                 + " , name=" + country[5] + "]");
 
		}
                
                int j = 0;
                
                for (int i = 0 ; i < header.length ; i++)
                {
                    String temp = header[i].toLowerCase();
                    if (!temp.equals("time") && !temp.equals("std"))
                    {
                        filteredHeader.add(header[i]);
                    }
                }
 
	} catch (FileNotFoundException e) {
		e.printStackTrace();
	} catch (IOException e) {
		e.printStackTrace();
	} finally {
		if (br != null) {
			try {
				br.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
        
        PopulateDropDownList(filteredHeader);
 
	//System.out.println("Done");
    }
    
    private void PopulateDropDownList(Vector<String> header)
    {
       // int length = header.length;
        
        //fields = new JComboBox(header.toArray(new String[header.size()]));
        //= new MutableComboBoxModel();
       MutableComboBoxModel model = (MutableComboBoxModel)fields.getModel();
       fields.removeAllItems();
       MutableComboBoxModel mutableModel = (MutableComboBoxModel) model;
       
       for (String str : header) {
            mutableModel.addElement(str);
        }
      
        this.setVisible(true);
    }
    public static void addTrendline(Plot2DPanel plotPanel, boolean removePrev, double[] xArr,
            double[] yArr)
	{
		if (removePrev)
			plotPanel.removePlot(trendline);
		
	
		trendline = plotPanel.addLinePlot("final", xArr, yArr);
	}
    
    private int GetSelectedIndex()
    {
        int index = 1;
        String sel = (String)fields.getSelectedItem();
        for (int i = 0 ; i < header.length ; i ++)
        {
            if (header[i].equals(sel))
            {
                index = i;
            }
        }
        return index;
    }
    private void PlotGraph()
    {
        String y = "time";
        String x = (String)fields.getSelectedItem();
        
        ArrayList<Double> yaxis = ReadCSV(GetSelectedIndex());
        ArrayList<Double> xaxis = ReadCSV(0); // read time
        
        double[] xArr = new double[xaxis.size()];
        int i = 0;
        for (double d : xaxis)
        {
            xArr[i++] = d;
        }
        
        double[] yArr = new double[yaxis.size()];
        i = 0;
        for (double d : yaxis)
        {
            yArr[i++] = d;
        }
        
        plotPanel.removeAllPlots();

		// add a the initial data to the PlotPanel
		plotPanel.addLinePlot("X-Y", xArr, yArr);
		
		// show the initial trendline
		//addTrendline(plotPanel, false, xArr, yArr);
		
		// put the PlotPanel in a JFrame, as a JPanel
                plotPanel.setVisible(true);
        
        
    }
 
    ArrayList<Double>  ReadCSV(int index)
    {
        String csvFile = filename;
	BufferedReader br = null;
	String line = "";
	String cvsSplitBy = "\\s+";
        ArrayList<Double> yaxis = new ArrayList<>();
 
	try {
 
		br = new BufferedReader(new FileReader(csvFile));
                br.readLine(); //waste first line
               //int index = 3;
                
                
                
                
		while ((line = br.readLine()) != null) {
 
		        // use comma as separator
                    
			String[] country = line.split(cvsSplitBy);
                        
                        if (!country[index].toLowerCase().equals("nil"))
                        {
                            yaxis.add(Double.valueOf(country[index]));
                        }
                        else // Do not ignore NILS
                        {
                            yaxis.add(0.00);
                        }
                        
//			System.out.println("Country [code= " + country[4] 
//                                 + " , name=" + country[5] + "]");
 
		}
 
	}
        catch (NumberFormatException e)
        {
            e.printStackTrace();
        }
        catch (FileNotFoundException e) {
		e.printStackTrace();
	} catch (IOException e) {
		e.printStackTrace();
	} finally {
		if (br != null) {
			try {
				br.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
        return yaxis;
    }
    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        setMinimumSize(new java.awt.Dimension(600, 560));
        setPreferredSize(new java.awt.Dimension(600, 560));

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 600, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 560, Short.MAX_VALUE)
        );
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    // End of variables declaration//GEN-END:variables

        @Override
        public void actionPerformed(ActionEvent ae) {
            if(ae.getSource() == browse)
            {
                if (!Browse().equals(""))
                {
                  
                  ReadCSVHeader();
                }
            }
            
            if (ae.getSource() == plot)
            {
                PlotGraph();
            }
        }
    }
}
