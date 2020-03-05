import java.awt.Dimension
import javax.swing.JFrame

object Main extends App {
  val frame = new JFrame("Graphics IDE")
  frame.setSize(new Dimension(600, 400))
  frame.setLocationRelativeTo(null)
  frame.setVisible(true)
}