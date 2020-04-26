sealed abstract class CustomList[T]

case class Nil[T]() extends CustomList[T]
case class Cons[T](head: T, tail: CustomList[T]) extends CustomList[T]

def drawCircleRec(centre_x: Int, centre_y: Int, x: Int, y: Int, p: () => Int, cont: () => CustomList[(Int, Int)]): CustomList[(Int, Int)] = {
  if(x < y) {
    return cont();
  }
  var points = () => Cons((x + centre_x, y + centre_y), Cons((-x + centre_x, y + centre_y), Cons((x + centre_x, -y + centre_y), Cons((-x + centre_x, -y + centre_y), cont()))))
  var points2 = () => points()
  if(x != y) {
    points2 = () => Cons((y + centre_x, x + centre_y), Cons((-y + centre_x, x + centre_y), Cons((y + centre_x, -x + centre_y), Cons((-y + centre_x, -x + centre_y), points()))))
  }
  val pp = p()
  println(s"p: $pp")
  if(p() <= 0) {
    drawCircleRec(centre_x, centre_y, x, y + 1, () => p() + 2 * y + 1, points2)
  } else {
    drawCircleRec(centre_x, centre_y, x-1, y + 1, () => p() + 2 * y - 2 * x + 1, points2)
  }
}

def drawCircle(centre_x: Int, centre_y: Int, radius: Int) = {
    var xx = radius
    if ((1 - radius) > 0) {
      xx = xx - 1
    }
    drawCircleRec(centre_x, centre_y, xx, 1, () => 1 - radius, () => Nil())

}

var res = drawCircle(0,0,3).asInstanceOf[Cons[(Int,Int)]]

while(!res.tail.isInstanceOf[Nil[(Int,Int)]]) {
  println(res.head)
  res = res.tail.asInstanceOf[Cons[(Int,Int)]]
}

println(res.head)