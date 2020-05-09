import java.awt.{Color, Graphics}

import javax.swing.JPanel

class CustomCanvas extends JPanel {

  var canvasElements: CustomList[CanvasElement] = Nil()

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)

    var allCoords: CustomList[Coordinate] = Nil()
    for (x <- 0 until canvasElements.length()) {
      canvasElements(x) match {
        case at: TextAt =>
          val textAt = at
          g.drawString(textAt.text, textAt.coordinates(0).x, textAt.coordinates(0).y)
        case fill: Fill =>
          g.setColor(mapToColor(fill.color))
          drawCoordinates(fill.coordinatesToBeColored,g)
          g.setColor(Color.BLACK)
          allCoords = allCoords.merge(fill.elementToBeFilled.coordinates)
        case _ => allCoords = allCoords.merge(canvasElements(x).coordinates)
      }
    }

    drawCoordinates(allCoords, g)
  }

  def drawCoordinates(coordinates: CustomList[Coordinate], g: Graphics): Unit = {
    for (i <- 0 until coordinates.length()) {
      g.fillRect(coordinates(i).x, coordinates(i).y, 1, 1)
    }
  }

  def paintPublic(elements: CustomList[CanvasElement]): Unit = {
    canvasElements = elements
    repaint()
  }

  def mapToColor(colorAsString: String) : Color = {
    colorAsString.toLowerCase() match {
      case "red" => Color.RED
      case "blue" => Color.BLUE
      case "black" => Color.BLACK
      case "green" => Color.GREEN
      case "yellow" => Color.YELLOW
      case "white" => Color.WHITE
      case "orange" => Color.ORANGE
      case "gray" => Color.GRAY
      case "pink" => Color.PINK
      case _ => Color.BLACK
    }
  }
  def clear(): Unit = {
    canvasElements = Nil()
    repaint()
  }

}