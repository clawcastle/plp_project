import java.awt.event.{KeyEvent, KeyListener}
import java.awt.{Color, Dimension, GridBagConstraints, GridBagLayout}

import javax.swing._

class UIAssembler extends KeyListener {
  val g = new GridBagConstraints()
  val frameDimension = new Dimension(800, 600)
  var textArea: JTextArea = _

  def startUI(): Unit = {
    g.fill = GridBagConstraints.BOTH
    val frame = createFrame()

    val mainPanel = new JPanel(new GridBagLayout())
    mainPanel.add(createDrawingArea(), g)

    mainPanel.add(createTextArea(), g)

    mainPanel.add(createErrorArea(), g)
    mainPanel.setBorder(BorderFactory.createEmptyBorder())

    frame.add(mainPanel)
    frame.setVisible(true)
  }

  def createFrame() : JFrame = {
    val frame = new JFrame("Graphics IDE")
    frame.setSize(frameDimension)
    frame.setLocationRelativeTo(null)
    frame.setResizable(false)
    frame
  }

  def createDrawingArea(): CustomCanvas = {
    g.weightx = 3
    g.weighty = 0.8

    val c = new CustomCanvas()
    c
  }

  def createTextArea(): JComponent = {
    g.gridx = 1
    g.weightx = 0.2
    g.weighty = 1.0
    g.gridheight = 2
    textArea = new JTextArea("This is text")
    textArea.addKeyListener(this)
    textArea.setLineWrap(true)
    val scrollPane = new JScrollPane(textArea)
    return scrollPane
  }

  def createErrorArea(): JComponent = {
    g.gridy = 1
    g.gridx = 0
    g.gridheight = 1
    g.weightx = 3
    g.weighty = 0.2
    val errorPanel = new JPanel()
    errorPanel.setBackground(Color.green)

    errorPanel
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

}
