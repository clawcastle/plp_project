object draw {
  def drawLine(x0: Int, y0: Int, x1: Int, y1: Int): CustomList[(Int, Int)] = {
    return (x0 to x1).map(x => (x,bresenhams(x0, y0, x1, y1, x))).toList
  }

  def bresenhams(x0: Int, y0: Int, x1: Int, y1: Int, x: Int): Int = {
    return ((y1 - y0) / (x1 - x0))*(x-x0) + y0
  }

  def drawCircle(centre_x: Int, centre_y: Int, radius: Int) = radius match {
    case radius <= 0 => Cons((centre_x, centre_y), Nil())
    case radius > 0 => drawCircleRec(centre_x, centre_y, 0, 0, () => 1 - radius, () => Nil())
  }

  def drawCircleRec(centre_x: Int, centre_y: Int, x: Int, y: Int, p: () => Int, cont: () => CustomList[(Int, Int)]): CustomList[(Int, Int)] = {
    if(x < y) {
      return cont();
    }
    var points = () => Cons((x + centre_x, y + centre_y), Cons((-x + centre_x, y + centre_y), Cons((x + centre_x, -y + centre_y), Cons((-x + centre_x, -y + centre_y), cont()))))

    if(x != y) {
      points = () => Cons((y + centre_x, x + centre_y), Cons((-y + centre_x, x + centre_y), Cons((y + centre_x, -x + centre_y), Cons((-y + centre_x, -x + centre_y), points()))))
    }

    if(p <= 0) {
      drawCircleRec(centre_x, centre_y, x, y, () => p() + 2 * y + 1, points)
    } else {
      drawCircleRec(centre_x, centre_y, x-1, y, () => p() + 2 * y - 2 * x + 1, points)
    }
  }
}
