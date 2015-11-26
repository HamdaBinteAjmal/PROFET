/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package EquationEditor.Display;
import EquationEditor.Output.OutputFormat;
import EquationEditor.Tree.BuildTree;
import EquationEditor.Tree.MathObject;
import EquationEditor.Tree.NaryOperator;
import MainInterface.MainForm;
import Nodes.EquationInfo;
import java.io.IOException;
import java.net.MalformedURLException;
import java.text.ParseException;
import javax.swing.border.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import javax.imageio.ImageIO;
import java.util.Stack;
import java.io.*;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.jdom.*;
import org.jdom.input.*;
/**
 *
 * @author Administrator
 */
public class EquationEditorPanel extends javax.swing.JPanel {

    /**
     * Creates new form EquationEditorPanel
     */
    private AddComponent addComponent;
    private static OutputFormat output;
    private static BuildTree buildTree;
    private static StatusBar statusBar;
    
    private MseSelectListener mouseSelectListener;
    private MseMotionSelectListener motionSelectListener;
    
    private boolean dragging;
    private InputComponent newComponent;
    private InputComponent[] inputComponents;
    
    private JPanel selectionObjects;
    private int firstLocation;
    private JPanel selectionLayer;
    
    private SAXBuilder builder;
    
    private Document componentFile;
    private Element inpComps;
    
    private  java.net.URL codeBase;
    private  java.applet.AppletContext appletContext;
    private  String language;
    private  String appletPath;
    private  String outputFormat;
    private  String openWithExpression;
    private  String openWithObject;
    private  boolean implicitMult;
    
    private LanguageManager langMan;
    MainForm.MainFormInner mainForm;
    
    private WSHelper xstream = new WSHelper();
    
    public EquationEditorPanel(MainForm.MainFormInner mF) {
        mainForm = mF;
        initComponents();
        initialize();
    }
    public void NewEquation()
    {
        jPanelWorkspace.removeAll();
        jPanelWorkspace.revalidate();
        jPanelWorkspace.repaint();
        openWithExpression = "diff(x,t)";
        openWithExpression(openWithExpression);
    }
    public void ExistingEquation(MathObject eqObj)
    {
        jPanelWorkspace.removeAll();
        jPanelWorkspace.revalidate();
        jPanelWorkspace.repaint();
        if (eqObj != null)
        {
            MathObject tree = eqObj;
            try {
                jPanelWorkspace.removeAll();
                addComponent.pasteTree(jPanelWorkspace, 0, tree, 0);
                addComponent.resetUndoRedo();
            } 
            catch (Exception ex) {
            JOptionPane.showMessageDialog(null, ex.toString(), "DragMath",
                    JOptionPane.ERROR_MESSAGE);
            }
        }
    }
    public void initialize()
    {
       // Set output format parameter
        outputFormat = "Lisp";
        language = "en";
                    
                    // Set hide menu parameter
        String hideMenu = "False";
                    
                    // Set hide menu parameter
        String hideButtons = "False";
                    
                    // Set custom toolbar
        String customToolBar = "0 1 2 | 3 4 | 5 6 7 | 8";
                    
        //String implicitMultStr = "True";
                    
        //String keepDoubleNumbersStr = "False";
                  
        implicitMult = true;
                                        
        boolean keepDoubleNumbers = false;
                    
        boolean hideDropDownMenu = true;
                  
                    
        boolean hideToolbar = true;
        //URL location = Test.class.getProtectionDomain().getCodeSource().getLocation();
        codeBase = EquationEditorPanel.class.getProtectionDomain().getCodeSource().getLocation();
        statusBar = new StatusBar(jLabelStatus);
        langMan = new LanguageManager(codeBase, statusBar);
        loadConfigFile();
                    langMan.loadLanguageFile(language);
                    
                    inputComponents = new InputComponent[100];
                    
                    addPaletteToolbarListeners(jTabbedPaneInput.getComponents());
                    addExtraComponents();
                    
                    // Perform custom toolbar code
                    parseCustomToolbarParam(customToolBar);
                    //IMPORTANT
                   // addCommandToolbarListeners(jToolBarEdit.getComponents());
                    //
                   output = new OutputFormat(statusBar, langMan, codeBase , implicitMult, keepDoubleNumbers);
                    buildTree = new BuildTree(langMan, inpComps);
                    output.readFormatFile(outputFormat);
                    
                    MouseListener textBoxListener = new EquationEditorPanel.MouseListenerTextBox();
                    addComponent = new AddComponent(inputComponents, jPanelWorkspace, buildTree, textBoxListener, statusBar, langMan, implicitMult, keepDoubleNumbers);
                    
                    // Set tabs icons
                    jTabbedPaneInput.setTitleAt(0, "");
                    jTabbedPaneInput.setIconAt(0, new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/operators.gif")));
                    jTabbedPaneInput.setTitleAt(1, "");
                    jTabbedPaneInput.setIconAt(1, new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/layout.gif")));
                    jTabbedPaneInput.setTitleAt(2, "");
                    jTabbedPaneInput.setIconAt(2, new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/fences.gif")));
                    jTabbedPaneInput.setTitleAt(3, "");
                    jTabbedPaneInput.setIconAt(3, new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/sin.gif")));
                    jTabbedPaneInput.setTitleAt(4, "");
                    jTabbedPaneInput.setIconAt(4, new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/calculus.gif")));
                    jTabbedPaneInput.setTitleAt(5, "");
                    jTabbedPaneInput.setIconAt(5, new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/greek.gif")));
//                    jTabbedPaneInput.setTitleAt(6, "");
//                    jTabbedPaneInput.setIconAt(6, new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/arrows.gif")));
//                    
                    dragging=false;
                    
                    mouseSelectListener = new MseSelectListener();
                    motionSelectListener = new MseMotionSelectListener(jPanelWorkspace, mouseSelectListener);
                    
                    jPanelWorkspace.addMouseListener(mouseSelectListener);
                    jPanelWorkspace.addMouseMotionListener(motionSelectListener);
                    
                    // class listens for mouse clicks on the workspace area.
                    jPanelWorkspace.addMouseListener(new java.awt.event.MouseAdapter() {
                        public void mouseClicked(java.awt.event.MouseEvent evt) {
                            if (evt.getClickCount() == 2) {
                                Point mousePos = evt.getPoint();
                                if (mousePos != null) {
                                    // Find the component where the mouse has been clicked
                                    JComponent component = (JComponent)jPanelWorkspace.findComponentAt(mousePos);
                                    motionSelectListener.clickSelect(component);
                                }
                            } else {
                                jPanelWorkspace.requestFocus();
                                addComponent(false, SwingUtilities.convertPoint(evt.getComponent(), evt.getPoint(), jPanelWorkspace));
                            }
                        }
                    });
                    
                    jPanelWorkspace.setLayout(new FlowLayout(FlowLayout.CENTER, 1, 5));
                    jPanelWorkspace.requestFocus();
                    //loadMenuText();
                    hideMenus(hideDropDownMenu, hideToolbar);
                    System.out.println("DragMath v0.7.9 successfully loaded");
                    
                    // Set implicit mult in menu
                    //jCheckBoxImplicitMult.setSelected(implicitMult);
                    
                    if (openWithExpression != null) {
                       // openWithExpression(openWithExpression);
                    } else //added by zmn to load from object
                    {
                        if(openWithObject != null) {
                            setFile(openWithObject);
                        }
                    }
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jLabelTooltip = new javax.swing.JLabel();
        jToolBarEdit2 = new javax.swing.JToolBar();
        jButtonUndo = new javax.swing.JButton();
        jButtonRedo = new javax.swing.JButton();
        jButtonCopy = new javax.swing.JButton();
        jButtonCut = new javax.swing.JButton();
        jButtonPaste = new javax.swing.JButton();
        jSeparator1 = new javax.swing.JToolBar.Separator();
        jButtonClear3 = new javax.swing.JButton();
        jButtonAddEquation = new javax.swing.JButton();
        jTabbedPaneInput = new javax.swing.JTabbedPane();
        jPanel3 = new javax.swing.JPanel();
        jToolBar1 = new javax.swing.JToolBar();
        jButton11 = new javax.swing.JButton();
        jButton26 = new javax.swing.JButton();
        jButton20 = new javax.swing.JButton();
        jButton32 = new javax.swing.JButton();
        jButton7 = new javax.swing.JButton();
        jToolBar10 = new javax.swing.JToolBar();
        jToolBar3 = new javax.swing.JToolBar();
        jButton19 = new javax.swing.JButton();
        jButton3 = new javax.swing.JButton();
        jButton4 = new javax.swing.JButton();
        jButton22 = new javax.swing.JButton();
        jButton23 = new javax.swing.JButton();
        jButtonMatrix1 = new javax.swing.JButton();
        jToolBar4 = new javax.swing.JToolBar();
        jButton15 = new javax.swing.JButton();
        jButton33 = new javax.swing.JButton();
        jButton73 = new javax.swing.JButton();
        jButton74 = new javax.swing.JButton();
        jToolBar6 = new javax.swing.JToolBar();
        jButtonSin = new javax.swing.JButton();
        jButtonCos = new javax.swing.JButton();
        jButtonTan = new javax.swing.JButton();
        jCheckBoxInverse = new javax.swing.JCheckBox();
        jCheckBoxHyp = new javax.swing.JCheckBox();
        jButton30 = new javax.swing.JButton();
        jButton31 = new javax.swing.JButton();
        jButton36 = new javax.swing.JButton();
        jToolBar8 = new javax.swing.JToolBar();
        jButton37 = new javax.swing.JButton();
        jPanel1 = new javax.swing.JPanel();
        jToolBar7 = new javax.swing.JToolBar();
        jButton38 = new javax.swing.JButton();
        jButton39 = new javax.swing.JButton();
        jButton40 = new javax.swing.JButton();
        jButton41 = new javax.swing.JButton();
        jButton42 = new javax.swing.JButton();
        jButton43 = new javax.swing.JButton();
        jButton44 = new javax.swing.JButton();
        jButton45 = new javax.swing.JButton();
        jButton46 = new javax.swing.JButton();
        jButton47 = new javax.swing.JButton();
        jToolBar2 = new javax.swing.JToolBar();
        jButton48 = new javax.swing.JButton();
        jButton49 = new javax.swing.JButton();
        jButton50 = new javax.swing.JButton();
        jButton51 = new javax.swing.JButton();
        jButton52 = new javax.swing.JButton();
        jButton53 = new javax.swing.JButton();
        jButton54 = new javax.swing.JButton();
        jButton55 = new javax.swing.JButton();
        jButton56 = new javax.swing.JButton();
        jButton57 = new javax.swing.JButton();
        jButton58 = new javax.swing.JButton();
        jButton59 = new javax.swing.JButton();
        jButton60 = new javax.swing.JButton();
        jButton61 = new javax.swing.JButton();
        jButton62 = new javax.swing.JButton();
        jButton63 = new javax.swing.JButton();
        jButton64 = new javax.swing.JButton();
        jButton66 = new javax.swing.JButton();
        jButton67 = new javax.swing.JButton();
        jButton68 = new javax.swing.JButton();
        jButton69 = new javax.swing.JButton();
        jButton70 = new javax.swing.JButton();
        jButton71 = new javax.swing.JButton();
        jButton72 = new javax.swing.JButton();
        jLabelStatus = new javax.swing.JLabel();
        jPanelWorkspace = new javax.swing.JPanel();
        jButtonClear = new javax.swing.JButton();
        jButtonSave = new javax.swing.JButton();

        setMinimumSize(new java.awt.Dimension(800, 560));
        setPreferredSize(new java.awt.Dimension(800, 560));
        setLayout(new org.netbeans.lib.awtextra.AbsoluteLayout());

        jLabelTooltip.setFont(new java.awt.Font("Arial", 0, 12)); // NOI18N
        jLabelTooltip.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        add(jLabelTooltip, new org.netbeans.lib.awtextra.AbsoluteConstraints(10, 420, 270, 30));

        jToolBarEdit2.setBorder(null);
        jToolBarEdit2.setFloatable(false);
        jToolBarEdit2.setEnabled(false);
        jToolBarEdit2.setMaximumSize(new java.awt.Dimension(600, 40));
        jToolBarEdit2.setMinimumSize(new java.awt.Dimension(600, 40));
        jToolBarEdit2.setPreferredSize(new java.awt.Dimension(600, 40));

        jButtonUndo.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/undo_24_h.png"))); // NOI18N
        jButtonUndo.setToolTipText("Undo");
        jButtonUndo.setFocusable(false);
        jButtonUndo.setMaximumSize(new java.awt.Dimension(24, 24));
        jButtonUndo.setMinimumSize(new java.awt.Dimension(24, 24));
        jButtonUndo.setName("UndoButton"); // NOI18N
        jButtonUndo.setOpaque(false);
        jButtonUndo.setPreferredSize(new java.awt.Dimension(24, 24));
        jButtonUndo.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jButtonUndoMouseClicked(evt);
            }
        });
        jButtonUndo.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonUndoActionPerformed(evt);
            }
        });
        jToolBarEdit2.add(jButtonUndo);

        jButtonRedo.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/redo_24_h.png"))); // NOI18N
        jButtonRedo.setToolTipText("Redo");
        jButtonRedo.setFocusable(false);
        jButtonRedo.setMaximumSize(new java.awt.Dimension(24, 24));
        jButtonRedo.setMinimumSize(new java.awt.Dimension(24, 24));
        jButtonRedo.setName("RedoButton"); // NOI18N
        jButtonRedo.setOpaque(false);
        jButtonRedo.setPreferredSize(new java.awt.Dimension(24, 24));
        jButtonRedo.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jButtonRedoMouseClicked(evt);
            }
        });
        jToolBarEdit2.add(jButtonRedo);

        jButtonCopy.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/copy_clipboard_24_h.png"))); // NOI18N
        jButtonCopy.setToolTipText("Copy");
        jButtonCopy.setBorder(javax.swing.BorderFactory.createEmptyBorder(4, 4, 4, 4));
        jButtonCopy.setFocusable(false);
        jButtonCopy.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonCopy.setName("CopyButton"); // NOI18N
        jButtonCopy.setOpaque(false);
        jButtonCopy.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButtonCopy.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jButtonCopyMouseClicked(evt);
            }
        });
        jButtonCopy.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonCopyActionPerformed(evt);
            }
        });
        jButtonCopy.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                jButtonCopyKeyPressed(evt);
            }
        });
        jToolBarEdit2.add(jButtonCopy);

        jButtonCut.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/cut_clipboard_24_h.png"))); // NOI18N
        jButtonCut.setToolTipText("Cut");
        jButtonCut.setFocusable(false);
        jButtonCut.setMaximumSize(new java.awt.Dimension(24, 24));
        jButtonCut.setMinimumSize(new java.awt.Dimension(24, 24));
        jButtonCut.setName("CutButton"); // NOI18N
        jButtonCut.setOpaque(false);
        jButtonCut.setPreferredSize(new java.awt.Dimension(24, 24));
        jButtonCut.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jButtonCutMouseClicked(evt);
            }
        });
        jToolBarEdit2.add(jButtonCut);

        jButtonPaste.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/paste_clipboard_24_h.png"))); // NOI18N
        jButtonPaste.setToolTipText("Paste");
        jButtonPaste.setFocusable(false);
        jButtonPaste.setMaximumSize(new java.awt.Dimension(24, 24));
        jButtonPaste.setMinimumSize(new java.awt.Dimension(24, 24));
        jButtonPaste.setName("PasteButton"); // NOI18N
        jButtonPaste.setOpaque(false);
        jButtonPaste.setPreferredSize(new java.awt.Dimension(24, 24));
        jButtonPaste.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jButtonPasteMouseClicked(evt);
            }
        });
        jButtonPaste.addFocusListener(new java.awt.event.FocusAdapter() {
            public void focusGained(java.awt.event.FocusEvent evt) {
                jButtonPasteFocusGained(evt);
            }
        });
        jToolBarEdit2.add(jButtonPaste);
        jToolBarEdit2.add(jSeparator1);

        jButtonClear3.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/cancel.png"))); // NOI18N
        jButtonClear3.setToolTipText("Clear Workspace");
        jButtonClear3.setFocusable(false);
        jButtonClear3.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonClear3.setMaximumSize(new java.awt.Dimension(24, 24));
        jButtonClear3.setMinimumSize(new java.awt.Dimension(24, 24));
        jButtonClear3.setName("ClearMenu"); // NOI18N
        jButtonClear3.setOpaque(false);
        jButtonClear3.setPreferredSize(new java.awt.Dimension(24, 24));
        jButtonClear3.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButtonClear3.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jButtonClear3MouseClicked(evt);
            }
        });
        jButtonClear3.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonClear3ActionPerformed(evt);
            }
        });
        jToolBarEdit2.add(jButtonClear3);

        jButtonAddEquation.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/check_24x24.png"))); // NOI18N
        jButtonAddEquation.setToolTipText("Save this Equation");
        jButtonAddEquation.setFocusable(false);
        jButtonAddEquation.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAddEquation.setMaximumSize(new java.awt.Dimension(24, 24));
        jButtonAddEquation.setMinimumSize(new java.awt.Dimension(24, 24));
        jButtonAddEquation.setName("AddEquationMenu"); // NOI18N
        jButtonAddEquation.setOpaque(false);
        jButtonAddEquation.setPreferredSize(new java.awt.Dimension(24, 24));
        jButtonAddEquation.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButtonAddEquation.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jButtonAddEquationMouseClicked(evt);
            }
        });
        jButtonAddEquation.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonAddEquationActionPerformed(evt);
            }
        });
        jToolBarEdit2.add(jButtonAddEquation);

        add(jToolBarEdit2, new org.netbeans.lib.awtextra.AbsoluteConstraints(10, 0, 230, -1));

        jTabbedPaneInput.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jTabbedPaneInput.setTabLayoutPolicy(javax.swing.JTabbedPane.SCROLL_TAB_LAYOUT);
        jTabbedPaneInput.setPreferredSize(new java.awt.Dimension(600, 87));

        jPanel3.setBackground(new java.awt.Color(255, 255, 255));
        jPanel3.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jPanel3.setLayout(new java.awt.GridBagLayout());

        jToolBar1.setBackground(new java.awt.Color(255, 255, 255));
        jToolBar1.setBorder(null);
        jToolBar1.setFloatable(false);

        jButton11.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton11.setText("+");
        jButton11.setToolTipText("");
        jButton11.setBorder(javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5));
        jButton11.setFocusable(false);
        jButton11.setName("2-Add"); // NOI18N
        jButton11.setOpaque(false);
        jButton11.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButton11ActionPerformed(evt);
            }
        });
        jToolBar1.add(jButton11);

        jButton26.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton26.setText("−");
        jButton26.setToolTipText("");
        jButton26.setBorder(javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5));
        jButton26.setFocusable(false);
        jButton26.setName("3-Subtract"); // NOI18N
        jButton26.setOpaque(false);
        jToolBar1.add(jButton26);

        jButton20.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton20.setText("·");
        jButton20.setToolTipText("");
        jButton20.setBorder(javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5));
        jButton20.setFocusable(false);
        jButton20.setName("0-Multiply"); // NOI18N
        jButton20.setOpaque(false);
        jToolBar1.add(jButton20);

        jButton32.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton32.setText(",");
        jButton32.setToolTipText("");
        jButton32.setBorder(javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5));
        jButton32.setFocusable(false);
        jButton32.setName("4-Comma"); // NOI18N
        jButton32.setOpaque(false);
        jToolBar1.add(jButton32);

        jButton7.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton7.setText("=");
        jButton7.setToolTipText("");
        jButton7.setBorder(javax.swing.BorderFactory.createEmptyBorder(5, 5, 5, 5));
        jButton7.setFocusable(false);
        jButton7.setName("14-Equals"); // NOI18N
        jButton7.setOpaque(false);
        jToolBar1.add(jButton7);

        jPanel3.add(jToolBar1, new java.awt.GridBagConstraints());

        jToolBar10.setBackground(new java.awt.Color(255, 255, 255));
        jToolBar10.setBorder(null);
        jToolBar10.setFloatable(false);
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        jPanel3.add(jToolBar10, gridBagConstraints);

        jTabbedPaneInput.addTab("tab7", jPanel3);

        jToolBar3.setBackground(new java.awt.Color(255, 255, 255));
        jToolBar3.setFloatable(false);

        jButton19.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton19.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/division.gif"))); // NOI18N
        jButton19.setToolTipText("");
        jButton19.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton19.setFocusable(false);
        jButton19.setName("1-Divide"); // NOI18N
        jButton19.setOpaque(false);
        jToolBar3.add(jButton19);

        jButton3.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton3.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/square root.gif"))); // NOI18N
        jButton3.setToolTipText("");
        jButton3.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton3.setFocusable(false);
        jButton3.setName("5-SquareRoot"); // NOI18N
        jButton3.setOpaque(false);
        jToolBar3.add(jButton3);

        jButton4.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton4.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/n-th root.gif"))); // NOI18N
        jButton4.setToolTipText("");
        jButton4.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton4.setFocusable(false);
        jButton4.setName("6-NthRoot"); // NOI18N
        jButton4.setOpaque(false);
        jToolBar3.add(jButton4);

        jButton22.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton22.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/superscript.gif"))); // NOI18N
        jButton22.setToolTipText("");
        jButton22.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton22.setFocusable(false);
        jButton22.setName("7-Power"); // NOI18N
        jButton22.setOpaque(false);
        jToolBar3.add(jButton22);

        jButton23.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton23.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/subscript.gif"))); // NOI18N
        jButton23.setToolTipText("");
        jButton23.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton23.setFocusable(false);
        jButton23.setName("8-Subscript"); // NOI18N
        jButton23.setOpaque(false);
        jToolBar3.add(jButton23);

        jButtonMatrix1.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/function.gif"))); // NOI18N
        jButtonMatrix1.setToolTipText("");
        jButtonMatrix1.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButtonMatrix1.setFocusable(false);
        jButtonMatrix1.setName("50-Function"); // NOI18N
        jButtonMatrix1.setOpaque(false);
        jButtonMatrix1.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jButtonMatrix1MouseClicked(evt);
            }
        });
        jToolBar3.add(jButtonMatrix1);

        jTabbedPaneInput.addTab("tab3", jToolBar3);

        jToolBar4.setBackground(new java.awt.Color(255, 255, 255));
        jToolBar4.setFloatable(false);

        jButton15.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton15.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/roundbr.gif"))); // NOI18N
        jButton15.setToolTipText("");
        jButton15.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton15.setFocusable(false);
        jButton15.setName("31-BracketsRnd"); // NOI18N
        jButton15.setOpaque(false);
        jToolBar4.add(jButton15);

        jButton33.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton33.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/absolute.gif"))); // NOI18N
        jButton33.setToolTipText("");
        jButton33.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton33.setFocusable(false);
        jButton33.setName("34-Abs"); // NOI18N
        jButton33.setOpaque(false);
        jToolBar4.add(jButton33);

        jButton73.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton73.setText("max");
        jButton73.setToolTipText("");
        jButton73.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton73.setFocusable(false);
        jButton73.setName("47-Max"); // NOI18N
        jButton73.setOpaque(false);
        jToolBar4.add(jButton73);

        jButton74.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton74.setText("min");
        jButton74.setToolTipText("");
        jButton74.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton74.setFocusable(false);
        jButton74.setName("48-Min"); // NOI18N
        jButton74.setOpaque(false);
        jToolBar4.add(jButton74);

        jTabbedPaneInput.addTab("tab4", jToolBar4);

        jToolBar6.setBackground(new java.awt.Color(255, 255, 255));
        jToolBar6.setFloatable(false);

        jButtonSin.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButtonSin.setText("sin");
        jButtonSin.setToolTipText("");
        jButtonSin.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButtonSin.setFocusable(false);
        jButtonSin.setName("21-Sine"); // NOI18N
        jButtonSin.setOpaque(false);
        jToolBar6.add(jButtonSin);

        jButtonCos.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButtonCos.setText("cos");
        jButtonCos.setToolTipText("");
        jButtonCos.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButtonCos.setFocusable(false);
        jButtonCos.setName("22-Cosine"); // NOI18N
        jButtonCos.setOpaque(false);
        jToolBar6.add(jButtonCos);

        jButtonTan.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButtonTan.setText("tan");
        jButtonTan.setToolTipText("");
        jButtonTan.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButtonTan.setFocusable(false);
        jButtonTan.setName("23-Tangent"); // NOI18N
        jButtonTan.setOpaque(false);
        jToolBar6.add(jButtonTan);

        jCheckBoxInverse.setText("Inverse");
        jCheckBoxInverse.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jCheckBoxInverse.setMargin(new java.awt.Insets(0, 0, 0, 0));
        jCheckBoxInverse.setOpaque(false);
        jCheckBoxInverse.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jCheckBoxInverseMouseClicked(evt);
            }
        });
        jToolBar6.add(jCheckBoxInverse);

        jCheckBoxHyp.setText("Hyperbolic");
        jCheckBoxHyp.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jCheckBoxHyp.setMargin(new java.awt.Insets(0, 0, 0, 0));
        jCheckBoxHyp.setOpaque(false);
        jCheckBoxHyp.addMouseListener(new java.awt.event.MouseAdapter() {
            public void mouseClicked(java.awt.event.MouseEvent evt) {
                jCheckBoxHypMouseClicked(evt);
            }
        });
        jToolBar6.add(jCheckBoxHyp);

        jButton30.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton30.setText("log");
        jButton30.setToolTipText("");
        jButton30.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton30.setFocusable(false);
        jButton30.setName("27-Logarithm"); // NOI18N
        jButton30.setOpaque(false);
        jToolBar6.add(jButton30);

        jButton31.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton31.setText("ln");
        jButton31.setToolTipText("");
        jButton31.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton31.setFocusable(false);
        jButton31.setName("28-NaturalLogarithm"); // NOI18N
        jButton31.setOpaque(false);
        jToolBar6.add(jButton31);

        jButton36.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton36.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/e.gif"))); // NOI18N
        jButton36.setToolTipText("");
        jButton36.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton36.setFocusable(false);
        jButton36.setName("29-Exp"); // NOI18N
        jButton36.setOpaque(false);
        jToolBar6.add(jButton36);

        jTabbedPaneInput.addTab("tab6", jToolBar6);

        jToolBar8.setBackground(new java.awt.Color(255, 255, 255));
        jToolBar8.setFloatable(false);

        jButton37.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton37.setIcon(new javax.swing.ImageIcon(getClass().getResource("/EquationEditor/Display/Images/differential.gif"))); // NOI18N
        jButton37.setToolTipText("");
        jButton37.setBorder(javax.swing.BorderFactory.createEmptyBorder(6, 6, 6, 6));
        jButton37.setFocusable(false);
        jButton37.setName("46-Differential"); // NOI18N
        jButton37.setOpaque(false);
        jToolBar8.add(jButton37);

        jTabbedPaneInput.addTab("tab8", jToolBar8);

        jPanel1.setBackground(new java.awt.Color(255, 255, 255));
        jPanel1.setLayout(new java.awt.GridBagLayout());

        jToolBar7.setBackground(new java.awt.Color(255, 255, 255));
        jToolBar7.setFloatable(false);

        jButton38.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton38.setText("Γ");
        jButton38.setToolTipText("");
        jButton38.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton38.setFocusable(false);
        jButton38.setName("35-GreekLetter"); // NOI18N
        jButton38.setOpaque(false);
        jToolBar7.add(jButton38);

        jButton39.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton39.setText("Δ");
        jButton39.setToolTipText("");
        jButton39.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton39.setFocusable(false);
        jButton39.setName("35-GreekLetter"); // NOI18N
        jButton39.setOpaque(false);
        jToolBar7.add(jButton39);

        jButton40.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton40.setText("Θ");
        jButton40.setToolTipText("");
        jButton40.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton40.setFocusable(false);
        jButton40.setName("35-GreekLetter"); // NOI18N
        jButton40.setOpaque(false);
        jToolBar7.add(jButton40);

        jButton41.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton41.setText("Ξ");
        jButton41.setToolTipText("");
        jButton41.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton41.setFocusable(false);
        jButton41.setName("35-GreekLetter"); // NOI18N
        jButton41.setOpaque(false);
        jToolBar7.add(jButton41);

        jButton42.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton42.setText("Π");
        jButton42.setToolTipText("");
        jButton42.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton42.setFocusable(false);
        jButton42.setName("35-GreekLetter"); // NOI18N
        jButton42.setOpaque(false);
        jToolBar7.add(jButton42);

        jButton43.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton43.setText("Σ");
        jButton43.setToolTipText("");
        jButton43.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton43.setFocusable(false);
        jButton43.setName("35-GreekLetter"); // NOI18N
        jButton43.setOpaque(false);
        jToolBar7.add(jButton43);

        jButton44.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton44.setText("γ");
        jButton44.setToolTipText("");
        jButton44.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton44.setFocusable(false);
        jButton44.setName("35-GreekLetter"); // NOI18N
        jButton44.setOpaque(false);
        jToolBar7.add(jButton44);

        jButton45.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton45.setText("Φ");
        jButton45.setToolTipText("");
        jButton45.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton45.setFocusable(false);
        jButton45.setName("35-GreekLetter"); // NOI18N
        jButton45.setOpaque(false);
        jToolBar7.add(jButton45);

        jButton46.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton46.setText("Ψ");
        jButton46.setToolTipText("");
        jButton46.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton46.setFocusable(false);
        jButton46.setName("35-GreekLetter"); // NOI18N
        jButton46.setOpaque(false);
        jToolBar7.add(jButton46);

        jButton47.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton47.setText("Ω");
        jButton47.setToolTipText("");
        jButton47.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton47.setFocusable(false);
        jButton47.setName("35-GreekLetter"); // NOI18N
        jButton47.setOpaque(false);
        jToolBar7.add(jButton47);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.gridx = 0;
        gridBagConstraints.gridy = 1;
        jPanel1.add(jToolBar7, gridBagConstraints);

        jToolBar2.setBackground(new java.awt.Color(255, 255, 255));
        jToolBar2.setFloatable(false);

        jButton48.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton48.setText("α");
        jButton48.setToolTipText("");
        jButton48.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton48.setFocusable(false);
        jButton48.setName("35-GreekLetter"); // NOI18N
        jButton48.setOpaque(false);
        jToolBar2.add(jButton48);

        jButton49.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton49.setText("β");
        jButton49.setToolTipText("");
        jButton49.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton49.setFocusable(false);
        jButton49.setName("35-GreekLetter"); // NOI18N
        jButton49.setOpaque(false);
        jToolBar2.add(jButton49);

        jButton50.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton50.setText("γ");
        jButton50.setToolTipText("");
        jButton50.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton50.setFocusable(false);
        jButton50.setName("35-GreekLetter"); // NOI18N
        jButton50.setOpaque(false);
        jToolBar2.add(jButton50);

        jButton51.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton51.setText("δ");
        jButton51.setToolTipText("");
        jButton51.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton51.setFocusable(false);
        jButton51.setName("35-GreekLetter"); // NOI18N
        jButton51.setOpaque(false);
        jToolBar2.add(jButton51);

        jButton52.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton52.setText("ε");
        jButton52.setToolTipText("");
        jButton52.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton52.setFocusable(false);
        jButton52.setName("35-GreekLetter"); // NOI18N
        jButton52.setOpaque(false);
        jToolBar2.add(jButton52);

        jButton53.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton53.setText("ζ");
        jButton53.setToolTipText("");
        jButton53.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton53.setFocusable(false);
        jButton53.setName("35-GreekLetter"); // NOI18N
        jButton53.setOpaque(false);
        jToolBar2.add(jButton53);

        jButton54.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton54.setText("η");
        jButton54.setToolTipText("");
        jButton54.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton54.setFocusable(false);
        jButton54.setName("35-GreekLetter"); // NOI18N
        jButton54.setOpaque(false);
        jToolBar2.add(jButton54);

        jButton55.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton55.setText("θ");
        jButton55.setToolTipText("");
        jButton55.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton55.setFocusable(false);
        jButton55.setName("35-GreekLetter"); // NOI18N
        jButton55.setOpaque(false);
        jToolBar2.add(jButton55);

        jButton56.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton56.setText("ι");
        jButton56.setToolTipText("");
        jButton56.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton56.setFocusable(false);
        jButton56.setName("35-GreekLetter"); // NOI18N
        jButton56.setOpaque(false);
        jToolBar2.add(jButton56);

        jButton57.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton57.setText("κ");
        jButton57.setToolTipText("");
        jButton57.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton57.setFocusable(false);
        jButton57.setName("35-GreekLetter"); // NOI18N
        jButton57.setOpaque(false);
        jToolBar2.add(jButton57);

        jButton58.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton58.setText("λ");
        jButton58.setToolTipText("");
        jButton58.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton58.setFocusable(false);
        jButton58.setName("35-GreekLetter"); // NOI18N
        jButton58.setOpaque(false);
        jToolBar2.add(jButton58);

        jButton59.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton59.setText("μ");
        jButton59.setToolTipText("");
        jButton59.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton59.setFocusable(false);
        jButton59.setName("35-GreekLetter"); // NOI18N
        jButton59.setOpaque(false);
        jToolBar2.add(jButton59);

        jButton60.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton60.setText("ν");
        jButton60.setToolTipText("");
        jButton60.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton60.setFocusable(false);
        jButton60.setName("35-GreekLetter"); // NOI18N
        jButton60.setOpaque(false);
        jToolBar2.add(jButton60);

        jButton61.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton61.setText("ξ");
        jButton61.setToolTipText("");
        jButton61.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton61.setFocusable(false);
        jButton61.setName("35-GreekLetter"); // NOI18N
        jButton61.setOpaque(false);
        jToolBar2.add(jButton61);

        jButton62.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton62.setText("ο");
        jButton62.setToolTipText("");
        jButton62.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton62.setFocusable(false);
        jButton62.setName("35-GreekLetter"); // NOI18N
        jButton62.setOpaque(false);
        jToolBar2.add(jButton62);

        jButton63.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton63.setText("π");
        jButton63.setToolTipText("");
        jButton63.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton63.setFocusable(false);
        jButton63.setName("35-GreekLetter"); // NOI18N
        jButton63.setOpaque(false);
        jToolBar2.add(jButton63);

        jButton64.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton64.setText("ρ");
        jButton64.setToolTipText("");
        jButton64.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton64.setFocusable(false);
        jButton64.setName("35-GreekLetter"); // NOI18N
        jButton64.setOpaque(false);
        jToolBar2.add(jButton64);

        jButton66.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton66.setText("σ");
        jButton66.setToolTipText("");
        jButton66.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton66.setFocusable(false);
        jButton66.setName("35-GreekLetter"); // NOI18N
        jButton66.setOpaque(false);
        jToolBar2.add(jButton66);

        jButton67.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton67.setText("τ");
        jButton67.setToolTipText("");
        jButton67.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton67.setFocusable(false);
        jButton67.setName("35-GreekLetter"); // NOI18N
        jButton67.setOpaque(false);
        jToolBar2.add(jButton67);

        jButton68.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton68.setText("υ");
        jButton68.setToolTipText("");
        jButton68.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton68.setFocusable(false);
        jButton68.setName("35-GreekLetter"); // NOI18N
        jButton68.setOpaque(false);
        jToolBar2.add(jButton68);

        jButton69.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton69.setText("φ");
        jButton69.setToolTipText("");
        jButton69.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton69.setFocusable(false);
        jButton69.setName("35-GreekLetter"); // NOI18N
        jButton69.setOpaque(false);
        jToolBar2.add(jButton69);

        jButton70.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton70.setText("χ");
        jButton70.setToolTipText("");
        jButton70.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton70.setFocusable(false);
        jButton70.setName("35-GreekLetter"); // NOI18N
        jButton70.setOpaque(false);
        jToolBar2.add(jButton70);

        jButton71.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton71.setText("ψ");
        jButton71.setToolTipText("");
        jButton71.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton71.setFocusable(false);
        jButton71.setName("35-GreekLetter"); // NOI18N
        jButton71.setOpaque(false);
        jToolBar2.add(jButton71);

        jButton72.setFont(new java.awt.Font("Lucida Sans Unicode", 0, 14)); // NOI18N
        jButton72.setText("ω");
        jButton72.setToolTipText("");
        jButton72.setBorder(javax.swing.BorderFactory.createEmptyBorder(3, 3, 3, 3));
        jButton72.setFocusable(false);
        jButton72.setName("35-GreekLetter"); // NOI18N
        jButton72.setOpaque(false);
        jToolBar2.add(jButton72);

        jPanel1.add(jToolBar2, new java.awt.GridBagConstraints());

        jTabbedPaneInput.addTab("tab6", jPanel1);

        add(jTabbedPaneInput, new org.netbeans.lib.awtextra.AbsoluteConstraints(10, 40, 540, 100));

        jLabelStatus.setFont(new java.awt.Font("Arial", 0, 12)); // NOI18N
        jLabelStatus.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        add(jLabelStatus, new org.netbeans.lib.awtextra.AbsoluteConstraints(280, 420, 270, 30));

        jPanelWorkspace.setBackground(new java.awt.Color(255, 255, 255));
        jPanelWorkspace.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        jPanelWorkspace.setMaximumSize(new java.awt.Dimension(600, 250));
        jPanelWorkspace.setMinimumSize(new java.awt.Dimension(600, 250));
        jPanelWorkspace.setPreferredSize(new java.awt.Dimension(600, 250));
        jPanelWorkspace.addContainerListener(new java.awt.event.ContainerAdapter() {
            public void componentAdded(java.awt.event.ContainerEvent evt) {
                jPanelWorkspaceComponentAdded(evt);
            }
        });
        jPanelWorkspace.addKeyListener(new java.awt.event.KeyAdapter() {
            public void keyPressed(java.awt.event.KeyEvent evt) {
                jPanelWorkspaceKeyPressed(evt);
            }
        });
        add(jPanelWorkspace, new org.netbeans.lib.awtextra.AbsoluteConstraints(10, 150, 540, 260));

        jButtonClear.setText("Clear Workspace");
        jButtonClear.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonClearActionPerformed(evt);
            }
        });
        add(jButtonClear, new org.netbeans.lib.awtextra.AbsoluteConstraints(80, 460, -1, -1));

        jButtonSave.setText("Save and Add Another Equation");
        jButtonSave.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jButtonSaveActionPerformed(evt);
            }
        });
        add(jButtonSave, new org.netbeans.lib.awtextra.AbsoluteConstraints(330, 460, -1, -1));
    }// </editor-fold>//GEN-END:initComponents

    private void jButtonUndoMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonUndoMouseClicked
        addComponent.undoState();
    }//GEN-LAST:event_jButtonUndoMouseClicked

    private void jButtonRedoMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonRedoMouseClicked
        addComponent.redoState();
    }//GEN-LAST:event_jButtonRedoMouseClicked

    private void jButtonCutMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonCutMouseClicked
        try {
            addComponent.cut(jPanelWorkspace, buildTree);
        } catch (ParseException ex) {
            JOptionPane.showMessageDialog(null, langMan.readLangFile("Cut") + ex.getMessage(), "DragMath", JOptionPane.ERROR_MESSAGE);
        }
    }//GEN-LAST:event_jButtonCutMouseClicked

    private void jButton11ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButton11ActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_jButton11ActionPerformed

    private void jButtonMatrix1MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonMatrix1MouseClicked
        // TODO add your handling code here:
    }//GEN-LAST:event_jButtonMatrix1MouseClicked

    private void jCheckBoxInverseMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jCheckBoxInverseMouseClicked
        changeTrigButtons();
    }//GEN-LAST:event_jCheckBoxInverseMouseClicked

    private void jCheckBoxHypMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jCheckBoxHypMouseClicked
        changeTrigButtons();
    }//GEN-LAST:event_jCheckBoxHypMouseClicked

    private void jPanelWorkspaceComponentAdded(java.awt.event.ContainerEvent evt) {//GEN-FIRST:event_jPanelWorkspaceComponentAdded

        statusBar.println("");
    }//GEN-LAST:event_jPanelWorkspaceComponentAdded

    private void jPanelWorkspaceKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jPanelWorkspaceKeyPressed

        if (evt.getKeyCode() == java.awt.event.KeyEvent.VK_DELETE || evt.getKeyCode() == java.awt.event.KeyEvent.VK_BACK_SPACE) {
            addComponent.delete(jPanelWorkspace);
        } else {
            // If workspace is blank
            if (jPanelWorkspace.getComponentCount() == 0) {
                TextBox newBox = addComponent.createBox(false);
                jPanelWorkspace.add(newBox);
                newBox.requestFocusInWindow();
                newBox.setText(String.valueOf(evt.getKeyChar()));
                jPanelWorkspace.revalidate();
            }
        }
    }//GEN-LAST:event_jPanelWorkspaceKeyPressed

    private void jButtonClearActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonClearActionPerformed
        // TODO add your handling code here:
        jPanelWorkspace.removeAll();
        jPanelWorkspace.revalidate();
        jPanelWorkspace.repaint();
        openWithExpression(openWithExpression);
    }//GEN-LAST:event_jButtonClearActionPerformed

    private void jButtonClear3MouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonClear3MouseClicked
        // TODO add your handling code here:
        jPanelWorkspace.removeAll();
        jPanelWorkspace.revalidate();
        jPanelWorkspace.repaint();
    }//GEN-LAST:event_jButtonClear3MouseClicked

    private void jButtonClear3ActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonClear3ActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_jButtonClear3ActionPerformed

    private void jButtonUndoActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonUndoActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_jButtonUndoActionPerformed

    private void jButtonAddEquationMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonAddEquationMouseClicked
        // TODO add your handling code here:
//        String equationStr = output();
//        JOptionPane.showMessageDialog(this, equationStr);
//        MathObject obj = null;
//         try {
//            obj = buildTree.generateTree(jPanelWorkspace, false, 0, 0);
//            
//        } catch (ParseException ex) {
//            JOptionPane.showMessageDialog(null, langMan.readLangFile("Tree") + ex.getMessage(), "DragMath", JOptionPane.ERROR_MESSAGE);
//        }
//         if (obj != null){
//             mainForm.AddEquation(obj, equationStr, inputComponents);
//         }
    }//GEN-LAST:event_jButtonAddEquationMouseClicked

    private void SaveEquation()
    {
         // TODO add your handling code here:
        MathObject tree = output();
        // String equationStr = output();
        //JOptionPane.showMessageDialog(this, equationStr);
        EquationInfo eq = new EquationInfo();
        eq = output.ParseEquationTree(tree);
        MathObject obj = null;
         try {
            obj = buildTree.generateTree(jPanelWorkspace, false, 0, 0);
            
        } catch (ParseException ex) {
            JOptionPane.showMessageDialog(null, langMan.readLangFile("Tree") + ex.getMessage(), "DragMath", JOptionPane.ERROR_MESSAGE);
        }
         if (obj != null){
             mainForm.AddEquation(eq);//obj, equationStr, inputComponents);
             statusBar.println("Equation Saved");
         }
         
    }
    private void jButtonAddEquationActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonAddEquationActionPerformed
       SaveEquation();
    }//GEN-LAST:event_jButtonAddEquationActionPerformed

    private void jButtonSaveActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonSaveActionPerformed
        // TODO add your handling code here:
        SaveEquation();
        NewEquation();
    }//GEN-LAST:event_jButtonSaveActionPerformed

    private void jButtonCopyActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jButtonCopyActionPerformed
        // TODO add your handling code here:
    }//GEN-LAST:event_jButtonCopyActionPerformed

    private void jButtonCopyMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonCopyMouseClicked
        // TODO add your handling code here:
          try {
            addComponent.copy(jPanelWorkspace, buildTree);
        } catch (ParseException ex) {
            JOptionPane.showMessageDialog(null, langMan.readLangFile("Copy") + ex.getMessage(), "DragMath", JOptionPane.ERROR_MESSAGE);
        }
    }//GEN-LAST:event_jButtonCopyMouseClicked

    private void jButtonCopyKeyPressed(java.awt.event.KeyEvent evt) {//GEN-FIRST:event_jButtonCopyKeyPressed
        // TODO add your handling code here:
    }//GEN-LAST:event_jButtonCopyKeyPressed

    private void jButtonPasteFocusGained(java.awt.event.FocusEvent evt) {//GEN-FIRST:event_jButtonPasteFocusGained
        // TODO add your handling code here:
    }//GEN-LAST:event_jButtonPasteFocusGained

    private void jButtonPasteMouseClicked(java.awt.event.MouseEvent evt) {//GEN-FIRST:event_jButtonPasteMouseClicked
        // TODO add your handling code here:
        addComponent.paste();
    }//GEN-LAST:event_jButtonPasteMouseClicked

public void changeTrigButtons() {
        if (jCheckBoxInverse.isSelected()) {
            if (jCheckBoxHyp.isSelected()) {
                jButtonCos.setText(inputComponents[43].getDisplayText());
                jButtonTan.setText(inputComponents[44].getDisplayText());
                jButtonSin.setText(inputComponents[42].getDisplayText());
                jButtonCos.setText(inputComponents[43].getDisplayText());
                jButtonTan.setText(inputComponents[44].getDisplayText());
                jButtonSin.setName(inputComponents[42].getTag());
                jButtonCos.setName(inputComponents[43].getTag());
                jButtonTan.setName(inputComponents[44].getTag());
            } else {
                jButtonSin.setText(inputComponents[24].getDisplayText());
                jButtonCos.setText(inputComponents[25].getDisplayText());
                jButtonTan.setText(inputComponents[26].getDisplayText());
                jButtonSin.setName(inputComponents[24].getTag());
                jButtonCos.setName(inputComponents[25].getTag());
                jButtonTan.setName(inputComponents[26].getTag());
            }
        } else {
            if (jCheckBoxHyp.isSelected()) {
                jButtonSin.setText(inputComponents[39].getDisplayText());
                jButtonCos.setText(inputComponents[40].getDisplayText());
                jButtonTan.setText(inputComponents[41].getDisplayText());
                jButtonSin.setName(inputComponents[39].getTag());
                jButtonCos.setName(inputComponents[40].getTag());
                jButtonTan.setName(inputComponents[41].getTag());
            } else {
                jButtonSin.setText(inputComponents[21].getDisplayText());
                jButtonCos.setText(inputComponents[22].getDisplayText());
                jButtonTan.setText(inputComponents[23].getDisplayText());
                jButtonSin.setName(inputComponents[21].getTag());
                jButtonCos.setName(inputComponents[22].getTag());
                jButtonTan.setName(inputComponents[23].getTag());
            }
        }
    }
    
    /**
     * @param args the command line arguments
     */
    
    public void hideMenus(boolean hideMenu, boolean hideToolbar) {
//        
//        if (hideMenu) {
//            jMenuBar.setVisible(false);
//        }
//        if (hideToolbar) {
//            jPanelToolbar.setVisible(false);
//        }
//        jToolBarEdit2.setVisible(false);
    }
    
    public void parseCustomToolbarParam(String toolbarParam) {
        
//        String curNumber = "";
//        
//        for (int i=0; i < toolbarParam.length(); i++) {
//            
//            char nextChar = toolbarParam.charAt(i);
//            
//            if (nextChar == '|') {
//                
//                if (curNumber != "") {
//                    // Add button for current number
//                    addButtonForNumber(curNumber);
//                    curNumber = "";
//                }
//                // Add separator
//                JSeparator jsept = new JSeparator(SwingConstants.VERTICAL);
//                jsept.setMaximumSize(new Dimension(2, 20));
//                jToolBarEdit.add(jsept);
//                
//                
//            } else if (nextChar == ' ') {
//                // Space
//                if (curNumber != "") {
//                    // Add button for current number
//                    addButtonForNumber(curNumber);
//                    curNumber = "";
//                }
//            } else {
//                curNumber = curNumber + nextChar;
//            }
//        }
//        
//        if (curNumber != "") {
//            // Add button for current number
//            addButtonForNumber(curNumber);
//            curNumber = "";
//        }
    }
    
    public void addButtonForNumber(String number) {
//        try {
//            
//            int bttnNum = Integer.parseInt(number);
//            
//            if (bttnNum == 0)
//                jToolBarEdit.add(jButtonClear);
//            
//            if (bttnNum == 1)
//                jToolBarEdit.add(jButtonLoad);
//            
//            if (bttnNum == 2)
//                jToolBarEdit.add(jButtonSave);
//            
//            if (bttnNum == 3)
//                jToolBarEdit.add(jButtonUndo);
//            
//            if (bttnNum == 4)
//                jToolBarEdit.add(jButtonRedo);
//            
//            if (bttnNum == 5)
//                jToolBarEdit.add(jButtonCut);
//            
//            if (bttnNum == 6)
//                jToolBarEdit.add(jButtonCopy);
//            
//            if (bttnNum == 7)
//                jToolBarEdit.add(jButtonPaste);
//            
//            if (bttnNum == 8)
//                jToolBarEdit.add(jButtonExport);
//            
//        } catch (NumberFormatException ex) {
//            // Not a parseable number
//        }
    }
    
    public void openWithExpression(String expression) {
        Stack outputStack;
        try {
            outputStack = BuildTree.parseString(expression, new Stack());
            if (outputStack.size() > 1) {
                MathObject tree = (MathObject)outputStack.pop();
                BuildTree.toTree(tree, outputStack);
                addComponent.pasteTree(jPanelWorkspace, 0, tree, 0);
                System.out.println("Expression loaded");
            }
        } catch (org.nfunk.jep.ParseException ex) {
            TextBox newBox = addComponent.createBox(false);
            jPanelWorkspace.add(newBox);
            newBox.requestFocusInWindow();
            newBox.setText(expression);
            jPanelWorkspace.revalidate();
            statusBar.println(langMan.readLangFile("ParseExp"));
        }
    }
    
    public String getFile() {
        String retstr = "";
        try {
            MathObject mobj =
                    buildTree.generateTree(jPanelWorkspace, false, 0, 0);
            retstr = xstream.OToS(mobj);
        } catch (ParseException ex) {
            JOptionPane.showMessageDialog(null, ex.getMessage(),
                    "DragMath", JOptionPane.ERROR_MESSAGE);
        }
        return retstr;
    }
    
    public void setFile(String mathstr) {
        MathObject tree = null;
        try {
            
            tree = (MathObject) xstream.SToO(mathstr);
            jPanelWorkspace.removeAll();
            addComponent.pasteTree(jPanelWorkspace, 0, tree, 0);
            addComponent.resetUndoRedo();
        } catch (Exception ex) {
            JOptionPane.showMessageDialog(null, ex.toString(), "DragMath",
                    JOptionPane.ERROR_MESSAGE);
        }
    }
    
    
    public void loadConfigFile() {
        builder = new SAXBuilder();
        try{
            componentFile = builder.build(this.getClass().getResourceAsStream("/EquationEditor/Display/CompConfig.xml"));
            inpComps = componentFile.getRootElement();
        } catch (IOException ex) {
            JOptionPane.showMessageDialog(null, "Error loading internal component file - please correct", "DragMath", JOptionPane.ERROR_MESSAGE);
        } catch (JDOMException ex) {
            JOptionPane.showMessageDialog(null, "Error loading internal component file - please correct", "DragMath", JOptionPane.ERROR_MESSAGE);
        }
    }
    
    /**
     * Adds a MouseListener to each JButton in each toolbar in the palette, and creates a JLabel for each JButton,
     * and stores it in the <code>labels</code> array.
     * @param components the array of toolbars, each containing JButtons
     */
    public void addPaletteToolbarListeners(Component[] components) {
        int i=0;
        
        while ( i < components.length) {
            if (components[i].getClass().getName().equals("javax.swing.JPanel")) {
                JPanel temp = (JPanel)components[i];
                addPaletteToolbarListeners(temp.getComponents());
            }
            if (components[i].getClass().getName().equals("javax.swing.JToolBar")) {
                JToolBar temp = (JToolBar)components[i];
                addPaletteToolbarListeners(temp.getComponents());
            }
            if (components[i].getClass().getName().equals("javax.swing.JButton")) {
                JButton button = (JButton)components[i];
                addToComponentArray(button.getName());
                components[i].addMouseListener(new EquationEditorPanel.MouseListenerPaletteToolbar());
            }
            i++;
        }
    }
    
    /** Adds a MouseListener to each JButton in the commands toolbar
     *@param components the array of components on the toolbar
     */
    public void addCommandToolbarListeners(Component[] components) {
        int i=0;
        while ( i < components.length) {
            if (components[i].getClass().getName().equals("javax.swing.JButton") ||
                    components[i].getClass().getName().equals("javax.swing.JCheckBox")) {
                
                //components[i].setName(langMan.readLangFile(components[i].getName()));
                components[i].addMouseListener(new java.awt.event.MouseAdapter() {
                    public void mouseEntered(java.awt.event.MouseEvent evt) {
                        jLabelTooltip.setText(langMan.readLangFile(evt.getComponent().getName()));
                    }
                    public void mouseExited(java.awt.event.MouseEvent evt) {
                        jLabelTooltip.setText("");
                    }
                });
                
            }
            i++;
        }
    }
    
    public void addToComponentArray(String name) {
        Toolkit toolkit = Toolkit.getDefaultToolkit();
        Element comp = inpComps.getChild(addComponent.getName(name));
        
        BufferedImage originalImage = null;
        Cursor newCursor = null;
        if (comp.getAttributeValue("iconFileName").equals("null")) {
        } else {
            try {
                originalImage = ImageIO.read(getClass().getResource("/EquationEditor/Display/Images/" + comp.getAttributeValue("iconFileName")));
            } catch (IOException ex) {
                ex.printStackTrace();
            }
            
            int w = originalImage.getWidth(this);
            int h = originalImage.getHeight(this);
            int bestW = toolkit.getBestCursorSize(w, h).width;
            int bestH = toolkit.getBestCursorSize(w, h).height;
            
            BufferedImage resizedImage = new BufferedImage(bestW,bestH,BufferedImage.TYPE_INT_ARGB);
            Graphics2D g = resizedImage.createGraphics();
            g.drawImage(originalImage,0,0,null);
            g.dispose();
            Point centerPoint = new Point(w/2,h/2);
            newCursor = toolkit.createCustomCursor(resizedImage, centerPoint, "Cursor");
        }
        
        inputComponents[addComponent.getID(name)] = new InputComponent(Integer.parseInt(comp.getAttributeValue("ID")),
                Integer.parseInt(comp.getAttributeValue("group")),
                comp.getText(),
                langMan.readLangFile(addComponent.getName(name)),
                newCursor, name,
                originalImage);
    }
    
    
    public void addExtraComponents() {
        addToComponentArray("24-ArcSine");
        addToComponentArray("25-ArcCosine");
        addToComponentArray("26-ArcTangent");
        addToComponentArray("39-SineH");
        addToComponentArray("40-CosineH");
        addToComponentArray("41-TanH");
        addToComponentArray("42-ArcSineH");
        addToComponentArray("43-ArcCosineH");
        addToComponentArray("44-ArcTanH");
        
        InputComponent unaryMinus = new InputComponent(30,3,"-", "Unary Minus", null, "30-UMinus", null);
        inputComponents[30]= unaryMinus;
    }
    
    
    public String getMathExpression() {
        String expression = "Failed to get expression";
        try {
            expression = output.GetPreFixString(buildTree.generateTree(jPanelWorkspace, false, 0, 0));
        } catch (ParseException ex) {
            JOptionPane.showMessageDialog(null, langMan.readLangFile(output.getOutputFormat()) + ex.getMessage(), "DragMath", JOptionPane.ERROR_MESSAGE);
        }
        return expression;
    }
    
    public String getMathExpressionForURL() {
        String expression = "Failed to get expression";
        try {
            expression = output.GetPreFixString(buildTree.generateTree(jPanelWorkspace, false, 0, 0));
            try {
                expression = java.net.URLEncoder.encode(expression, "UTF-8");
            } catch (UnsupportedEncodingException ex) {
                //ex.printStackTrace();
            }
        } catch (ParseException ex) {
            JOptionPane.showMessageDialog(null, langMan.readLangFile(output.getOutputFormat()) + ex.getMessage(), "DragMath", JOptionPane.ERROR_MESSAGE);
        }
        return expression;
    }
    
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButton11;
    private javax.swing.JButton jButton15;
    private javax.swing.JButton jButton19;
    private javax.swing.JButton jButton20;
    private javax.swing.JButton jButton22;
    private javax.swing.JButton jButton23;
    private javax.swing.JButton jButton26;
    private javax.swing.JButton jButton3;
    private javax.swing.JButton jButton30;
    private javax.swing.JButton jButton31;
    private javax.swing.JButton jButton32;
    private javax.swing.JButton jButton33;
    private javax.swing.JButton jButton36;
    private javax.swing.JButton jButton37;
    private javax.swing.JButton jButton38;
    private javax.swing.JButton jButton39;
    private javax.swing.JButton jButton4;
    private javax.swing.JButton jButton40;
    private javax.swing.JButton jButton41;
    private javax.swing.JButton jButton42;
    private javax.swing.JButton jButton43;
    private javax.swing.JButton jButton44;
    private javax.swing.JButton jButton45;
    private javax.swing.JButton jButton46;
    private javax.swing.JButton jButton47;
    private javax.swing.JButton jButton48;
    private javax.swing.JButton jButton49;
    private javax.swing.JButton jButton50;
    private javax.swing.JButton jButton51;
    private javax.swing.JButton jButton52;
    private javax.swing.JButton jButton53;
    private javax.swing.JButton jButton54;
    private javax.swing.JButton jButton55;
    private javax.swing.JButton jButton56;
    private javax.swing.JButton jButton57;
    private javax.swing.JButton jButton58;
    private javax.swing.JButton jButton59;
    private javax.swing.JButton jButton60;
    private javax.swing.JButton jButton61;
    private javax.swing.JButton jButton62;
    private javax.swing.JButton jButton63;
    private javax.swing.JButton jButton64;
    private javax.swing.JButton jButton66;
    private javax.swing.JButton jButton67;
    private javax.swing.JButton jButton68;
    private javax.swing.JButton jButton69;
    private javax.swing.JButton jButton7;
    private javax.swing.JButton jButton70;
    private javax.swing.JButton jButton71;
    private javax.swing.JButton jButton72;
    private javax.swing.JButton jButton73;
    private javax.swing.JButton jButton74;
    private javax.swing.JButton jButtonAddEquation;
    private javax.swing.JButton jButtonClear;
    private javax.swing.JButton jButtonClear3;
    private javax.swing.JButton jButtonCopy;
    private javax.swing.JButton jButtonCos;
    private javax.swing.JButton jButtonCut;
    private javax.swing.JButton jButtonMatrix1;
    private javax.swing.JButton jButtonPaste;
    private javax.swing.JButton jButtonRedo;
    private javax.swing.JButton jButtonSave;
    private javax.swing.JButton jButtonSin;
    private javax.swing.JButton jButtonTan;
    private javax.swing.JButton jButtonUndo;
    private javax.swing.JCheckBox jCheckBoxHyp;
    private javax.swing.JCheckBox jCheckBoxInverse;
    private javax.swing.JLabel jLabelStatus;
    private javax.swing.JLabel jLabelTooltip;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel3;
    private javax.swing.JPanel jPanelWorkspace;
    private javax.swing.JToolBar.Separator jSeparator1;
    private javax.swing.JTabbedPane jTabbedPaneInput;
    private javax.swing.JToolBar jToolBar1;
    private javax.swing.JToolBar jToolBar10;
    private javax.swing.JToolBar jToolBar2;
    private javax.swing.JToolBar jToolBar3;
    private javax.swing.JToolBar jToolBar4;
    private javax.swing.JToolBar jToolBar6;
    private javax.swing.JToolBar jToolBar7;
    private javax.swing.JToolBar jToolBar8;
    private javax.swing.JToolBar jToolBarEdit2;
    // End of variables declaration//GEN-END:variables
public void saveAsFile() {
        JFileChooser chooser = new JFileChooser();
        chooser.setFileFilter(new DrgmFileFilter());
        chooser.setDialogTitle(langMan.readLangFile("SaveExpression"));
        int returnVal = chooser.showSaveDialog(this);
        if(returnVal == JFileChooser.APPROVE_OPTION) {
            try {
                String filePath = chooser.getSelectedFile().getPath();
                if (DrgmFileFilter.isDrgmFile(chooser.getSelectedFile()) == false) {
                    filePath = filePath + ".drgm";
                }
                ObjectOutputStream expressionFile = new ObjectOutputStream(new FileOutputStream(filePath));
                expressionFile.writeObject(buildTree.generateTree(jPanelWorkspace, false, 0, 0));
                expressionFile.close();
                statusBar.println("Expression saved");
            } catch (FileNotFoundException ex) {
                JOptionPane.showMessageDialog(null, langMan.readLangFile("SavingExp"), "DragMath", JOptionPane.ERROR_MESSAGE);
            } catch (ParseException ex) {
                JOptionPane.showMessageDialog(null, langMan.readLangFile("SavingExp"), "DragMath", JOptionPane.ERROR_MESSAGE);
            } catch (IOException ex) {
                JOptionPane.showMessageDialog(null, langMan.readLangFile("SavingExp"), "DragMath", JOptionPane.ERROR_MESSAGE);
            }
        }
    }
    
 public void openFile() {
        JFileChooser chooser = new JFileChooser();
        chooser.setFileFilter(new DrgmFileFilter());
        chooser.setDialogTitle(langMan.readLangFile("LoadExpression"));
        int returnVal = chooser.showOpenDialog(this);
        if(returnVal == JFileChooser.APPROVE_OPTION) {
            MathObject tree = null;
            try {
                String temp = chooser.getSelectedFile().getPath();
                ObjectInputStream expressionFile = new ObjectInputStream(new FileInputStream(chooser.getSelectedFile().getPath()));
                tree = (MathObject) expressionFile.readObject();
                expressionFile.close();
                jPanelWorkspace.removeAll();
                addComponent.pasteTree(jPanelWorkspace, 0, tree, 0);
                addComponent.resetUndoRedo();
            } catch (FileNotFoundException ex) {
                JOptionPane.showMessageDialog(null, langMan.readLangFile("LoadingExp"), "DragMath", JOptionPane.ERROR_MESSAGE);
            } catch (ClassNotFoundException ex) {
                JOptionPane.showMessageDialog(null, langMan.readLangFile("LoadingExp"), "DragMath", JOptionPane.ERROR_MESSAGE);
            } catch (IOException ex) {
                JOptionPane.showMessageDialog(null, langMan.readLangFile("LoadingExp"), "DragMath", JOptionPane.ERROR_MESSAGE);
            }
        }
    }
    
    
    /** Sets the borders of all components in the display to red for JPanel, green for JLabel
     * and blue for <code>TextBox</code>, or sets all the borders to none
     * @param borders boolean to say whether to add borders or remove them
     * @param layer JComponent that contains the components to set the borders on
     */
    public void setBorders(boolean borders, JComponent layer) {
        Component[] components = layer.getComponents();
        int i=0;
        boolean panel=false;
        
        while ( i < components.length) {
            java.awt.Color colour = null;
            if (components[i].getClass().getName().equals("javax.swing.JPanel")) {
                colour = new java.awt.Color(java.awt.Color.RED.getRGB());
                panel=true;
            } else if (components[i].getClass().getName().equals("EquationEditor.Display.TextBox")) {
                colour = new java.awt.Color(java.awt.Color.BLUE.getRGB());
            } else {
                colour = new java.awt.Color(java.awt.Color.GREEN.getRGB());
            }
            
            JComponent component = (JComponent)components[i];
            if (components[i].getClass().getName().equals("javax.swing.JTextField")) {
                // ignore component, not for user to see
            } else {
                if (borders) {
                    component.setBorder(new LineBorder(colour));
                } else {
                    component.setBorder(javax.swing.BorderFactory.createEmptyBorder());
                    if (component.getClass().getName().equals("EquationEditor.Display.TextBox")) {
                        TextBox temp = (TextBox)component;
                        temp.setBorder(new EtchedBorder());
                        if (temp.getText().length() > 0) {
                            temp.setBorder(new EmptyBorder(temp.getInsets()));
                        }
                    }
                }
            }
            if (panel) {
                panel=false;
                setBorders(borders, component);
            }
            i++;
        }
    }
    
    
    public MathObject output() {
        String eq= "";
        MathObject tree = null;
        try {
            tree = addComponent.checkSelection(jPanelWorkspace, buildTree, null);
            // If a selection has been made
            if (tree == null)
            {
                tree = buildTree.generateTree(jPanelWorkspace, false, 0, 0);
                //eq = output.GetPreFixString(buildTree.generateTree(jPanelWorkspace, false, 0, 0));
            }
            
            //just get the RHS expression string
           
//        if (tree != null)
//        {
//           if (tree.getClass().getName().equals("EquationEditor.Tree.NaryOperator")) 
//           {
//               NaryOperator naryObj = (NaryOperator)tree;
//               String equal = inputComponents[naryObj.getID()].getDisplayText();
//               if (equal.equals("="))
//               {
//                   int i = naryObj.getSize();
//                   if (i == 2)
//                   {
//                       start = naryObj.getChild(0);
//                   }
//               }
//           }
//        }
//            // If a selection has been made
//            if (start != null) {
//                eq = output.GetPreFixString(start);
//            }
    
        } 
        catch (ParseException ex) {
            JOptionPane.showMessageDialog(null, ex.getMessage(), "DragMath", JOptionPane.ERROR_MESSAGE);
        }
        return tree;
    }
    
    public MathObject GetExpressionTree()
    {
      MathObject tree = null ;
         try {
            tree = addComponent.checkSelection(jPanelWorkspace, buildTree, null);
            // If a selection has been made
            if (tree == null) {
                buildTree.generateTree(jPanelWorkspace, false, 0, 0);
            }
         }
         catch (ParseException ex) {
            JOptionPane.showMessageDialog(null, ex.getMessage(), "DragMath", JOptionPane.ERROR_MESSAGE);
        }
         return tree;
         
    }
    
    
    
    /** Determines where to add the chosen component in the display
     * depending upon where the mouse has been clicked or released.
     */
    public void addComponent(boolean emptyWorkspace, Point mousePos) {
        setCursor(Cursor.getDefaultCursor());
        if (emptyWorkspace) {
            mousePos = new Point(0,0);
        }
        if (mousePos.getX() < 0 || mousePos.getX() > jPanelWorkspace.getWidth() || mousePos.getY() < 0 || mousePos.getY() > jPanelWorkspace.getHeight()) {
            mousePos=null;
        }
        if (dragging && mousePos != null) {
            
            // Find the component where the mouse has been clicked
            JComponent component = (JComponent)jPanelWorkspace.findComponentAt(mousePos);
            JPanel layer = null;
            if (component != jPanelWorkspace) {
                layer = (JPanel)component.getParent();
            } else {
                layer = jPanelWorkspace;
            }
            
            int n=0;
            int status=AddComponent.BLANK_WORKSPACE;
            boolean add=false;
            boolean layoutPanel=false;
            
            // If workspace isn't empty
            if (jPanelWorkspace.getComponentCount() > 0) {
                
                // If a blank space on workspace hasn't been clicked
                if (component != jPanelWorkspace) {
                    
                    Component[] components = layer.getComponents();
                    int i=0,j=0;
                    // Find component n-th order
                    while (i < components.length) {
                        if (component.equals(components[i])) j=i;
                        i++;
                    }
                    
                    // If component isn't a JPanel
                    if (component.getClass().getName() != "javax.swing.JPanel")  {
                        
                        if (component.getClass().getName() == "EquationEditor.Display.TextBox") {
                            // Component is a box
                            n=j;
                            status=AddComponent.ONTO_BOX;
                            add=true;
                        } else {
                            // Component is label
                            // If component is meant to be added onto
                            if (component.getName() != "") {
                                n=j;
                                status=AddComponent.ONTO_GRAPHIC;
                                add=true;
                            }
                        }
                    }
                    // A layer has been clicked
                    else {
                        // If layer is part of a layout component
                        if (component.getName() != "" && addComponent.getGroup(component.getName()) == 0) {
                            n=j;
                            status=AddComponent.ONTO_GRAPHIC;
                            add=true;
                            layer = (JPanel)component;
                            layoutPanel=true;
                        }
                    }
                }
                // Blank space clicked
                else {
                    // If operator, add operator to end of expression
                    if (newComponent.getGroup() == 1 || newComponent.getGroup() == 2) {
                        status=AddComponent.ONTO_BOX;
                        add=true;
                        n=layer.getComponentCount()-1;
                    }
                }
            }
            // Workspace is empty
            else {
                n=0;
                status=AddComponent.BLANK_WORKSPACE;
                add=true;
            }
            
            if (add) {
                int group = newComponent.getGroup();
                int ID = newComponent.getID();
                
                // Save current state for undo
                addComponent.saveState(true);
                
                // Component is layout
                if (group == AddComponent.LAYOUT) {
                    // Symbol is matrix
                    if (ID == 9) {
                        String[] values = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "10"};
                        boolean cancel=false;
                        String matrix_m = null;
                        String matrix_n = null;
                        matrix_m = (String)JOptionPane.showInputDialog(null, langMan.readLangFile("EnterRows"), langMan.readLangFile("MatrixDim"),JOptionPane.QUESTION_MESSAGE, null, values, "1");
                        if (matrix_m == null) {
                            cancel = true;
                        }
                        
                        if (cancel == false){
                            matrix_n = (String)JOptionPane.showInputDialog(null, langMan.readLangFile("EnterColumns"), langMan.readLangFile("MatrixDim"),JOptionPane.QUESTION_MESSAGE, null, values, "1");
                            if (matrix_n == null) {
                                cancel = true;
                            }
                        }
                        if (cancel == false) {
                            addComponent.addLayout(layer, n, newComponent, status, layoutPanel, Integer.parseInt(matrix_m), Integer.parseInt(matrix_n), null);
                        }
                    } else {
                        addComponent.addLayout(layer, n, newComponent, status, layoutPanel, 0, 0, null);
                    }
                }
                // Component is operator
                if (group == AddComponent.NARY || group == AddComponent.BINARY) {
                    addComponent.addOperator(layer, n, newComponent, status, layoutPanel, null);
                }
                // Component is function
                if (group == AddComponent.FUNCTION) {
                    addComponent.addFunction(layer, n, newComponent, status, layoutPanel, null);
                }
                // Component is symbol
                if (group == AddComponent.SYMBOL) {
                    addComponent.addSymbol(layer, n, newComponent, status);
                }
                // Component is grouping
                if (group == AddComponent.GROUPING) {
                    addComponent.addGrouping(layer, n, newComponent, status, layoutPanel, null);
                }
            }
        }
        dragging=false;
    }
    
    
    
    /** This class listens for mouse clicks on the TextBox object, and sets the cursor icon
     */
    class MouseListenerTextBox extends MouseAdapter {
        
        public MouseListenerTextBox() {
        }
        
        public void mouseClicked(MouseEvent e) {
            if (e.getClickCount() == 2) {
                motionSelectListener.clickSelect((JComponent)e.getSource());
            } else {
                addComponent(false, SwingUtilities.convertPoint(e.getComponent(), e.getPoint(), jPanelWorkspace));
            }
        }
        
        public void mouseEntered(MouseEvent e) {
            if (dragging) {
                TextBox temp = (TextBox)e.getComponent();
                temp.setCursor(newComponent.getCursor());
                //temp.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
            }
        }
        
        public void mouseExited(MouseEvent e) {
            TextBox temp = (TextBox)e.getComponent();
            temp.setCursor(Cursor.getDefaultCursor());
        }
    }
    
    
    /** Finds all the components in the display that have been selected
     * @param layer The current JPanel to look for selected components in
     * @param componentFound Boolean to say whether or not any selected components have been found,
     * set to false when first called
     */
    public void getSelection(JPanel layer, boolean componentFound) {
        Component[] components = layer.getComponents();
        int i=0;
        while (i < components.length) {
            Color colour = new Color(Color.LIGHT_GRAY.getRGB());
            
            // If components are highlighted i.e selected
            if (components[i].getBackground().equals(colour)) {
                if (componentFound == false) {
                    firstLocation = i;
                    selectionLayer = layer;
                    componentFound=true;
                }
                
                // If component is an argument panel, leave panel there and remove all components on it
                if (components[i].getClass().getName().equals("javax.swing.JPanel") && components[i].getName() == "") {
                    JPanel temp = (JPanel)components[i];
                    while (temp.getComponentCount() > 0) {
                        selectionObjects.add(temp.getComponent(0));
                    }
                    selectionLayer = temp;
                    firstLocation=0;
                } else {
                    selectionObjects.add(components[i]);
                }
            } else {
                // If component is JPanel that is not selected, search inside this JPanel
                if (components[i].getClass().getName().equals("javax.swing.JPanel")) {
                    getSelection((JPanel)components[i], componentFound);
                }
            }
            i++;
        }
    }

    
    
    /** Class that extends MouseAdapter and listens for MouseEvents on the JButtons in the toolbars
     */
    class MouseListenerPaletteToolbar extends MouseAdapter {
        
        private Point xy1;
        
        public MouseListenerPaletteToolbar() {
        }
        
        public void mousePressed(MouseEvent e) {
            xy1 = e.getPoint();
            newComponent = inputComponents[addComponent.getID(e.getComponent().getName())];
            
            dragging=true;
            
            // If workspace is empty
            if (jPanelWorkspace.getComponentCount() == 0) {
                addComponent(true, SwingUtilities.convertPoint(e.getComponent(), e.getPoint(), jPanelWorkspace));
            } else if (newComponent.getGroup() == AddComponent.SYMBOL) {
                dragging=false;
                JComponent focusComp = (JComponent)KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                if (focusComp.getClass().getName().equals("EquationEditor.Display.TextBox")) {
                    TextBox temp = (TextBox)focusComp;
                    JButton tmp = (JButton)e.getComponent();
                    temp.setText(temp.getText() + tmp.getText());
                }
            } else {
                // If component is not matrix or exponential
                if (newComponent.getID() != 9 && newComponent.getID() != 29) {
                    selectionObjects = addComponent.createPanel("");
                    
                    boolean selectionValid=true;
                    // Check selection is valid expression by trying to create tree from it
                    try {
                        addComponent.checkSelection(jPanelWorkspace, buildTree, newComponent);
                    } catch (ParseException ex) {
                        if (ex.getMessage().equals(("Replaced operator"))) {
                            
                        } else {
                            JOptionPane.showMessageDialog(null, langMan.readLangFile("Action") + ex.getMessage(), "DragMath", JOptionPane.ERROR_MESSAGE);
                        }
                        selectionValid=false;
                        dragging=false;
                    }
                    
                    if (selectionValid) {
                        
                        // Capture state before getSelection() may remove some components from the display
                        MathObject savedTree = null;
                        try {
                            savedTree = buildTree.generateTree(jPanelWorkspace, false, 0, 0);
                        } catch (ParseException ex) {
                        }
                        
                        getSelection(jPanelWorkspace, false);
                        
                        // If panel containting selected component isn't empty
                        if (selectionObjects.getComponentCount() > 0) {
                            
                            // Save the state previously captured
                            addComponent.saveState(savedTree);
                            
                            int group = newComponent.getGroup();
                            
                            // group is layout
                            if (group == AddComponent.LAYOUT) {
                                addComponent.addLayout(selectionLayer, firstLocation, newComponent, 3, false,0, 0, selectionObjects);
                            }
                            // group is operator
                            if (group == AddComponent.NARY || group == AddComponent.BINARY) {
                                // Minus
                                if (newComponent.getID() == 3) {
                                    
                                    // If component before minus is addition, then remove addition and treat as minus
                                    if (firstLocation > 0 && addComponent.getID(selectionLayer.getComponent(firstLocation - 1).getName()) == 2) {
                                        selectionLayer.remove(firstLocation - 1);
                                        selectionLayer.add(addComponent.createSymbol(inputComponents[3]), firstLocation - 1);
                                        addComponent.addGrouping(selectionLayer, firstLocation, inputComponents[31], 3, false, selectionObjects);
                                    } // Add unary minus
                                    else {
                                        newComponent = inputComponents[30];
                                        addComponent.addFunction(selectionLayer, firstLocation, newComponent, 3, false, selectionObjects);
                                    }
                                } else {
                                    addComponent.addOperator(selectionLayer, firstLocation, newComponent, 3, false, selectionObjects);
                                }
                            }
                            // group is function
                            if (group == AddComponent.FUNCTION) {
                                addComponent.addFunction(selectionLayer, firstLocation, newComponent, 3, false, selectionObjects);
                            }
                            // group is grouping
                            if (group == AddComponent.GROUPING) {
                                addComponent.addGrouping(selectionLayer, firstLocation, newComponent, 3, false, selectionObjects);
                            }
                            dragging=false;
                            MseSelectListener.deSelect(jPanelWorkspace);
                        }
                    }
                }
                if (dragging) {
                    setCursor(newComponent.getCursor());
                    //setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
                }
                
            }
        }
        
        
        public void mouseReleased(MouseEvent e) {
            if (e.getX() != xy1.getX() || e.getY() != xy1.getY()) {
                addComponent(false, SwingUtilities.convertPoint(e.getComponent(), e.getPoint(), jPanelWorkspace));
            }
        }
        
        public void mouseEntered(MouseEvent e) {
            JButton button = (JButton)e.getComponent();
            jLabelTooltip.setText(langMan.readLangFile(addComponent.getName(button.getName())));
            //.setText(inputComponents[addComponent.getID()].getTooltip());
        }
        
        public void mouseExited(MouseEvent e) {
            jLabelTooltip.setText("");
        }
    }
}

