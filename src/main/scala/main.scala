import java.awt.{BorderLayout, Color, Dimension, GridBagConstraints, GridBagLayout}

import javax.swing.{BorderFactory, JComponent, JFrame, JLabel, JPanel, JTextArea, JTextField}

object Main extends App {
  val g = new GridBagConstraints()
  g.fill = GridBagConstraints.BOTH
  val frame = new JFrame("Graphics IDE")

  val mainPanel = new JPanel(new GridBagLayout())
  mainPanel.setBackground(Color.red)

  mainPanel.add(createDrawingArea(), g)
  mainPanel.add(createTextArea(), g)
  mainPanel.add(createErrorArea(), g)

  mainPanel.setBorder(BorderFactory.createEmptyBorder())

  frame.add(mainPanel)
  frame.setSize(new Dimension(600, 400))
  frame.setLocationRelativeTo(null)
  frame.setVisible(true)

  def createDrawingArea(): JComponent = {
    g.weightx = 0.7
    g.weighty = 0.8
    val drawingArea = new JPanel()

    drawingArea.setBackground(Color.blue)
    return drawingArea;
  }

  def createTextArea(): JComponent = {
    g.gridx = 1
    g.weightx = 0.3
    g.weighty = 1.0
    g.gridheight = 2
    val textArea = new JTextArea("This is text")

    return textArea
  }

  def createErrorArea(): JComponent = {
    g.gridy = 1
    g.gridx = 0
    g.gridheight = 1
    g.weightx = 0.7
    g.weighty = 0.2
    val errorPanel = new JPanel()
    errorPanel.setBackground(Color.green)

    errorPanel
  }
}