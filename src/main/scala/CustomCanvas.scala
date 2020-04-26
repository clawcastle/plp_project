import java.awt.{Graphics, Graphics2D}
import java.awt.image.BufferedImage

import javax.swing.JPanel

class CustomCanvas extends JPanel {
  var intList: CustomList[(Int, Int)] = Nil()

  override def paint(g: _root_.java.awt.Graphics): Unit = {
    if(intList.isInstanceOf[Nil[(Int,Int)]]) {
      return;
    }
    val list = intList.asInstanceOf[Cons[(Int,Int)]]
    //val buf = new BufferedImage(100, 100, BufferedImage.TYPE_3BYTE_BGR)

    for(i <- 0 until list.length()) {
      g.fillRect(list(i)._1, list(i)._2, 1, 1)
    }
  }

  def paintPublic(coords: CustomList[(Int, Int)]): Unit = {
    intList = coords;
    super.repaint()
  }
}