import java.awt.{Graphics, Graphics2D}
import java.awt.image.BufferedImage

import javax.swing.JPanel

class CustomCanvas extends JPanel {
  var intList: List[(Int, Int)] = List[(Int,Int)]()

  override def paint(g: _root_.java.awt.Graphics): Unit = {
    //val buf = new BufferedImage(100, 100, BufferedImage.TYPE_3BYTE_BGR)
    for(i <- intList.indices) {
      g.fillRect(intList(i)._1, intList(i)._2, 1, 1)
    }
  }

  def paintPublic(coords: List[(Int, Int)]): Unit = {
    for(i <- coords.indices) {
      intList = coords(i) :: intList
    }
    super.repaint()
  }
}