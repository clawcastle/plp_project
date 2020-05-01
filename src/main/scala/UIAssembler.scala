import java.awt.event.{KeyEvent, KeyListener}
import java.awt.{Color, Dimension, GridBagConstraints, GridBagLayout}
import java.awt.BorderLayout

import javax.swing._
import javax.swing.BorderFactory
import javax.swing.JComponent
import javax.swing.JPanel

class UIAssembler extends KeyListener {
  val gbc = new GridBagConstraints()
  val frameDimension = new Dimension(800, 600)
  var textArea: JTextArea = _
  var errorText: JTextPane = _

  def startUI(): Unit = {
    val frame = createFrame()

    val mainPanel = new JPanel(new GridBagLayout())

    addComp(mainPanel, createDrawingArea(), x = 0, y = 0, gWidth = 1, gHeight = 1, GridBagConstraints.BOTH, weightx = 1, weighty = 0.9)
    addComp(mainPanel, createRightPanel(), x = 1, y = 0, gWidth = 1, gHeight = 2, GridBagConstraints.BOTH, weightx = 0, weighty = 1)
    addComp(mainPanel, createErrorArea(), x = 0, y = 1, gWidth = 1, gHeight = 1, GridBagConstraints.BOTH, weightx = 1, weighty = 0.1)

    mainPanel.setBorder(BorderFactory.createEmptyBorder())
    frame.add(mainPanel)
    frame.setVisible(true)
  }

  def createFrame() : JFrame = {
    val frame = new JFrame("Graphics IDE")
    frame.setSize(frameDimension)
    frame.setLocationRelativeTo(null)
    frame
  }

  def createDrawingArea(): CustomCanvas = {
    val c = new CustomCanvas()
    c
  }

  def createButtonPanel(): JComponent = {
    val buttonPanel = new JPanel(new BorderLayout(10, 10))
    val clearCanvasButton = new JButton("<html><center>Clear<br>Canvas</html>")
    val clearCommandsButton = new JButton("<html><center>Clear<br>Commands</html>")
    val drawButton = new JButton("Draw")
    drawButton.setSize(200, 200)
    drawButton.setToolTipText("Ctrl + Enter")

    buttonPanel.add(clearCanvasButton, BorderLayout.LINE_START)
    buttonPanel.add(clearCommandsButton, BorderLayout.CENTER )
    buttonPanel.add(drawButton, BorderLayout.LINE_END)
    buttonPanel
  }

  def createRightPanel(): JComponent = {
    val rightPanel = new JPanel(new BorderLayout(10, 10))
    rightPanel.add(createTextArea(), BorderLayout.CENTER)
    rightPanel.add(createButtonPanel(), BorderLayout.SOUTH)
    rightPanel.setBorder(BorderFactory.createTitledBorder("Command window"))
    rightPanel.setOpaque(true)
    rightPanel.setBackground(Color.WHITE)
    rightPanel
  }

  def createTextArea(): JComponent = {
    textArea = new JTextArea()
    textArea.addKeyListener(this)
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

  def addErrorTrace(s : String): Unit = {
    var text = errorText.getText()
    text = text + "\n" + s
    errorText.setText(text)
  }

  def keyReleased(e: KeyEvent): Unit = {
    if (e.isControlDown) {
      if (e.getKeyCode.equals(KeyEvent.VK_ENTER)){
          if (e.getKeyCode.equals(10)) {
            commandParser.parseCommands(textArea.getText)
          }
      }
    }
  }

  def keyTyped(e: KeyEvent): Unit = {
    //Do nothing
  }

  def keyPressed(e: KeyEvent): Unit = {
    //Do nothing
  }

  private def addComp(panel: JPanel, comp: JComponent, x: Int, y: Int, gWidth: Int, gHeight: Int, fill: Int, weightx: Double, weighty: Double): Unit = {
    gbc.gridx = x
    gbc.gridy = y
    gbc.gridwidth = gWidth
    gbc.gridheight = gHeight
    gbc.fill = fill
    gbc.weightx = weightx
    gbc.weighty = weighty
    panel.add(comp, gbc)
  }

}
