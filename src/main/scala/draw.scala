object draw {
  def drawLine(x0: Int, y0: Int, x1: Int, y1: Int): List[(Int, Int)] = {
    return (x0 to x1).map(x => (x,bresenhams(x0, y0, x1, y1, x))).toList
  }

  def bresenhams(x0: Int, y0: Int, x1: Int, y1: Int, x: Int): Int = {
    return ((y1 - y0) / (x1 - x0))*(x-x0) + y0
  }
}
