import java.awt.event.{ActionEvent, ActionListener, KeyEvent, KeyListener}
import java.awt._

import javax.swing.event.{UndoableEditEvent, UndoableEditListener}
import javax.swing.text.Document
import javax.swing.undo.{CannotUndoException, UndoManager}
import javax.swing.{BorderFactory, JComponent, JPanel, _}

class UIAssembler {
  val gbc = new GridBagConstraints()
  val normalKeyListener = new NormalDrawKeyListener
  val autoKeyListener = new AutoDrawKeyListener
  val undoManager: UndoManager = new UndoManager
  val frameDimension = new Dimension(900, 800)
  var textArea: JTextArea = _
  var document: Document = _
  var errorText: JTextPane = _
  var canvas: CustomCanvas = _
  var drawButton: JButton = _

  def startUI(): Unit = {
    val frame = createFrame()

    val mainPanel = new JPanel(new GridBagLayout())

    addComp(mainPanel, createDrawingArea(), gbc, x = 0, y = 0, gWidth = 1, gHeight = 1, GridBagConstraints.BOTH, weightx = 1, weighty = 0.9)
    addComp(mainPanel, createRightPanel(), gbc, x = 1, y = 0, gWidth = 1, gHeight = 2, GridBagConstraints.BOTH, weightx = 0, weighty = 1)
    addComp(mainPanel, createErrorArea(), gbc, x = 0, y = 1, gWidth = 1, gHeight = 1, GridBagConstraints.BOTH, weightx = 1, weighty = 0.1)

    mainPanel.setBorder(BorderFactory.createEmptyBorder())
    frame.add(mainPanel)
    frame.setVisible(true)
  }

  def createFrame(): JFrame = {
    val frame = new JFrame("Graphics IDE")
    frame.setSize(frameDimension)
    frame.setLocationRelativeTo(null)
    frame
  }

  def createDrawingArea(): CustomCanvas = {
    canvas = new CustomCanvas()
    canvas
  }

  def createButtonPanel(): JComponent = {
    val buttonPanel = new JPanel(new BorderLayout(10, 10))
    val clearCanvasButton = createClearCanvasBtn()
    val clearCommandsButton = createClearCommandsBtn()
    val drawButton = createDrawBtn()
    buttonPanel.add(clearCanvasButton, BorderLayout.LINE_START)
    buttonPanel.add(clearCommandsButton, BorderLayout.CENTER)
    buttonPanel.add(drawButton, BorderLayout.LINE_END)
    buttonPanel
  }

  def createClearCommandsBtn(): JButton = {
    val clearCommandsBtn = new JButton("<html><center>Clear<br>Commands</html>")
    clearCommandsBtn.addActionListener((actionEvent: ActionEvent) => {
      clearCommands()
    })
    clearCommandsBtn
  }

  def createClearCanvasBtn(): JButton = {
    val clearCanvasBtn = new JButton("<html><center>Clear<br>Canvas</html>")
    clearCanvasBtn.addActionListener((_: ActionEvent) => {
      clearCanvas()
    })
    clearCanvasBtn
  }

  def createDrawBtn(): JButton = {
    drawButton = new JButton("Draw")
    drawButton.setToolTipText("Ctrl + Enter")
    drawButton.addActionListener((_: ActionEvent) => {
      parseCommands()
    })
    drawButton
  }

  def clearCanvas(): Unit = {
    canvas.clear()
  }

  def clearCommands(): Unit = {
    textArea.setText("")
  }

  def createRightPanel(): JComponent = {
    val rightPanel = new JPanel(new GridBagLayout())
    val gbc = new GridBagConstraints()

    addComp(rightPanel, createTextArea(), gbc, x = 0, y = 0, gWidth = 1, gHeight = 2, GridBagConstraints.BOTH, weightx = 1, weighty = 1)
    gbc.anchor = GridBagConstraints.WEST
    addComp(rightPanel, createCheckBox(), gbc, x = 0, y = 3, gWidth = 1, gHeight = 1, GridBagConstraints.NONE, weightx = 1, weighty = 0)
    gbc.anchor = GridBagConstraints.CENTER
    addComp(rightPanel, createButtonPanel(), gbc, x = 0, y = 4, gWidth = 1, gHeight = 1, GridBagConstraints.NONE, weightx = 1, weighty = 0)

    rightPanel.setBorder(BorderFactory.createTitledBorder("Command window"))
    rightPanel.setOpaque(true)
    rightPanel.setBackground(Color.WHITE)
    rightPanel
  }

  def createCheckBox(): JCheckBox = {
    val checkBox = new JCheckBox("Enable auto-draw")
    checkBox.addActionListener((_: ActionEvent) => {
      if (checkBox.isSelected) {
        textArea.removeKeyListener(normalKeyListener)
        textArea.addKeyListener(autoKeyListener)
        drawButton.setEnabled(false)
      } else {
        textArea.removeKeyListener(autoKeyListener)
        textArea.addKeyListener(normalKeyListener)
        drawButton.setEnabled(true)
      }
    })
    checkBox
  }

  def createTextArea(): JComponent = {
    textArea = new JTextArea()
    document = textArea.getDocument

    document.addUndoableEditListener(new UndoableEditListener {
      override def undoableEditHappened(undoableEditEvent: UndoableEditEvent): Unit =
        {
          undoManager.addEdit(undoableEditEvent.getEdit)
        }
    })


    textArea.addKeyListener(normalKeyListener)
    textArea.setLineWrap(true)
    val scrollPane = new JScrollPane(textArea)
    scrollPane
  }

  def createErrorArea(): JComponent = {
    errorText = new JTextPane()
    errorText.setEditable(false)
    val scrollPane = new JScrollPane(errorText)
    scrollPane.setBorder(BorderFactory.createTitledBorder("Error trace"))
    scrollPane.setOpaque(true)
    scrollPane.setBackground(Color.WHITE)
    scrollPane.setAutoscrolls(true)
    scrollPane
  }

  def addErrorTrace(s: String): Unit = {
    var text = errorText.getText()
    text = text + "\n" + s
    errorText.setText(text)
  }

  def parseCommands(): Unit = {
    new Thread(() => {
      SwingUtilities.invokeLater(() => {
        try {
          errorText.setText("")
          drawShapes(CommandParser.parseCommands(textArea.getText))
        } catch {
          case e: Exception => updateErrorText(e)
        }
      })
    }).start()
  }

  def updateErrorText(e: Exception): Unit = {
    SwingUtilities.invokeLater(() => {errorText.setText(e.getMessage)});
  }

  def drawShapes(shapes: CustomList[CanvasElement]): Unit = {
    canvas.paintPublic(shapes)
  }

  def undo(): Unit = {
    try {
      if (undoManager.canUndo) {
        undoManager.undo()
      }
    } catch {
      case e: CannotUndoException => updateErrorText(e)
    }
  }

  private def addComp(panel: JPanel, comp: JComponent, gbc: GridBagConstraints, x: Int, y: Int, gWidth: Int, gHeight: Int, fill: Int, weightx: Double, weighty: Double): Unit = {
    gbc.gridx = x
    gbc.gridy = y
    gbc.gridwidth = gWidth
    gbc.gridheight = gHeight
    gbc.fill = fill
    gbc.weightx = weightx
    gbc.weighty = weighty
    panel.add(comp, gbc)
  }

  class AutoDrawKeyListener extends KeyListener {
    def keyReleased(e: KeyEvent): Unit = {
      parseCommands()
    }

    def keyTyped(e: KeyEvent): Unit = {
      //Do nothing
    }

    def keyPressed(e: KeyEvent): Unit = {
      //Do nothing
    }
  }

  class NormalDrawKeyListener extends KeyListener {
    def keyReleased(e: KeyEvent): Unit = {
      if (e.isControlDown) {
        if (e.getKeyCode.equals(KeyEvent.VK_ENTER)) {
          if (e.getKeyCode.equals(10)) {
            parseCommands()
          }
        } else if (e.getKeyCode.equals(KeyEvent.VK_Z)) {
          undo()
        }
      }
    }

    def keyTyped(e: KeyEvent): Unit = {
      //Do nothing
    }

    def keyPressed(e: KeyEvent): Unit = {
      //Do nothing
    }
  }



}


