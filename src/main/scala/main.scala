import java.awt.image.BufferedImage
import java.awt.{Color, Dimension, GridBagConstraints, GridBagLayout}
import javax.swing.{BorderFactory, JComponent, JFrame, JPanel, JTextArea}

object Main extends App {
  val g = new GridBagConstraints()
  g.fill = GridBagConstraints.BOTH
  val frame = new JFrame("Graphics IDE")
  val drawingArea = createDrawingArea()
  val mainPanel = new JPanel(new GridBagLayout())
  val command = "(LINE (0 0) (100 100))"

  var coords: CustomList[(Int,Int)] = Nil[(Int,Int)]();
  CustomList.range(1, 10).map(x => draw.drawCircle((x * 10) + 100, (x * 10) + 100, 100)).forEach(x => coords = coords.merge(x))
  //var coords = draw.drawCircle(100,100,50).merge(draw.drawCircle(150, 150, 80)).merge(draw.drawCircle(75, 75, 40))
  //val coords = commandParser.parseCommand(command)
  //val coords = draw.drawLine(0, 0, 200, 200)
  //mainPanel.setBackground(Color.red)

  mainPanel.add(drawingArea, g)
  mainPanel.add(createTextArea(), g)
  mainPanel.add(createErrorArea(), g)

  mainPanel.setBorder(BorderFactory.createEmptyBorder())

  frame.add(mainPanel)
  frame.setSize(new Dimension(600, 400))
  frame.setLocationRelativeTo(null)
  frame.setVisible(true)

  drawingArea.paintPublic(coords)

  def createDrawingArea(): CustomCanvas = {
    g.weightx = 0.7
    g.weighty = 0.8

    val c = new CustomCanvas()
    return c
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