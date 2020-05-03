import java.awt.{Color, Graphics, Graphics2D, Image}
import java.awt.image.BufferedImage

import javax.swing.JPanel

class CustomCanvas extends JPanel {
  var canvasElements: CustomList[CanvasElement] = Nil()

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)

    var allCoords: CustomList[Coordinate] = Nil()
    for (x <- 0 until canvasElements.length()) {
      canvasElements(x) match {
        case at: TextAt =>
          var textAt = at
          g.drawString(textAt.text, textAt.coordinates(0).x, textAt.coordinates(0).y)
        case fill: Fill =>
          g.setColor(fill.color)
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
    canvasElements = elements;
    repaint()
  }
}