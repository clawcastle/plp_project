import java.awt.{Graphics, Graphics2D, Image}
import java.awt.image.BufferedImage

import javax.swing.JPanel

class CustomCanvas extends JPanel {
  var coordinates: CustomList[Coordinate] = Nil()

  var img : Image = new BufferedImage(200, 200, 1);

  override def paintComponent(g : Graphics): Unit = {
    super.paintComponent(g)
    for(i <- 0 until coordinates.length()) {
      g.fillRect(coordinates(i).x, coordinates(i).y, 1, 1)
    }
  }

  def paintPublic(coords: CustomList[Coordinate]): Unit = {
    coordinates = coords;
    repaint()
  }
}