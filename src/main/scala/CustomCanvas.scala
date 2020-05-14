import java.awt.{Color, Graphics}

import javax.swing.{JPanel, SwingUtilities}

class CustomCanvas extends JPanel {

  var canvasElements: CustomList[CanvasElement] = Nil()
  var gridCoords: CustomList[Coordinate] = Nil()
  var shouldHighlight: Boolean = false

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)

    drawGrid(g, 30)
    drawCanvasElements(canvasElements,g)
  }

  def drawCanvasElements(canvasElements: CustomList[CanvasElement], g: Graphics): Unit =
  {
    if (canvasElements.length() == 0)
      return

    drawOldElements(canvasElements, g)
    drawLastElement(canvasElements(canvasElements.length()-1), g)
  }

  def drawOldElements(canvasElements: CustomList[CanvasElement], g: Graphics): Unit = {
    var oldCoords: CustomList[Coordinate] = Nil()
    for (x <- 0 until canvasElements.length()-1) {
      oldCoords = oldCoords.merge(handleElement(canvasElements(x), g))
    }
    drawCoordinates(oldCoords, g)
  }

  def drawLastElement(lastElement: CanvasElement, g: Graphics): Unit = {
    if (shouldHighlight)
      g.setColor(Color.RED)
    else
      g.setColor(Color.BLACK)

    var newCoords = handleElement(lastElement, g)
    drawCoordinates(newCoords, g)
  }

  def handleElement(element: CanvasElement, g: Graphics): CustomList[Coordinate] = {
    var coords: CustomList[Coordinate] = Nil()
    element match {
      case at: TextAt =>
        val textAt = at
        g.drawString(textAt.text, textAt.coordinates(0).x, textAt.coordinates(0).y)
      case fill: Fill =>
        val previousColor = g.getColor
        g.setColor(mapToColor(fill.color))
        drawCoordinates(fill.coordinatesToBeColored, g)
        g.setColor(previousColor)
        coords = coords.merge(fill.elementToBeFilled.coordinates)
      case drawObjects: DrawObjects =>
        g.setColor(mapToColor(drawObjects.color))
        drawCanvasElements(drawObjects.elements, g)
        g.setColor(Color.BLACK)
      case _ => coords = coords.merge(element.coordinates)
    }
    coords
  }

  def drawCoordinates(coordinates: CustomList[Coordinate], g: Graphics): Unit = {
    for (i <- 0 until coordinates.length()) {
      g.fillRect(coordinates(i).x, coordinates(i).y, 1, 1)
    }
  }

  def paintPublic(elements: CustomList[CanvasElement]): Unit = {
    canvasElements = elements
    new Thread(() => {
      SwingUtilities.invokeLater(new Runnable {
        override def run(): Unit = {
          shouldHighlight = true
          repaint()
        }
      })

      Thread.sleep(2000)

      SwingUtilities.invokeLater(() => {
        shouldHighlight = false
        repaint()
      })
    }).start()
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

  def findGridCoords(space: Int): CustomList[Coordinate] = {
    var coords : CustomList[Coordinate] = Nil[Coordinate]()
    for(y <- 1 to getHeight){
      for(x <- 1 to getWidth){
        if (y % space == 0) {
          coords = coords.append(new Coordinate(x, y))
        }
        else if (x % space == 0) {
          coords = coords.append(new Coordinate(x, y))
        }
      }
    }
    coords
  }

  def drawGrid(g : Graphics, space : Int): Unit = {
    if (gridCoords.isInstanceOf[Nil[Coordinate]]){
      gridCoords = findGridCoords(space)
    }

    g.setColor(Color.LIGHT_GRAY)
    drawCoordinates(gridCoords, g)
    g.setColor(Color.BLACK)
  }
}