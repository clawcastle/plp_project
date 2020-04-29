sealed abstract class CustomList[T];
case class Nil[T]() extends CustomList[T]
case class Cons[T](head: T, tail: CustomList[T]) extends CustomList[T]

def printList[T](list: CustomList[T]): Unit = {
  if(list.isInstanceOf[Nil[T]]) {
    return;
  }
  println("list")
  var l = list.asInstanceOf[Cons[T]];
  while(!l.tail.isInstanceOf[Nil[T]]) {
    println(l.head)
    l = l.tail.asInstanceOf[Cons[T]]
  }
  println(l.head)
}

def drawCircleRec(centre_x: Int, centre_y: Int, x: Int, yy: Int, p: () => Int, cont: () => CustomList[(Int, Int)]): CustomList[(Int, Int)] = {
  if(x < yy) {
    printList(cont())
    return cont();
  }
  var y = yy + 1;
  printList(cont())
  var points = () => Cons((x + centre_x, y + centre_y), Cons((-x + centre_x, y + centre_y), Cons((x + centre_x, -y + centre_y), Cons((-x + centre_x, -y + centre_y), cont()))))
  var points2 = () => points()
  if(x != y) {
    points2 = () => Cons((y + centre_x, x + centre_y), Cons((-y + centre_x, x + centre_y), Cons((y + centre_x, -x + centre_y), Cons((-y + centre_x, -x + centre_y), points()))))
  }
  val pp = p()
  println(s"p: $pp, x: $x, y: $y")

  if(p() <= 0) {
    drawCircleRec(centre_x, centre_y, x, y, () => p() + 2 * y + 1, points2)
  } else {
    drawCircleRec(centre_x, centre_y, x-1, y, () => p() + 2 * y - 2 * x + 1, points2)
  }
}

def drawCircle(centre_x: Int, centre_y: Int, radius: Int) = {
    var xx = radius
    if ((1 - radius) > 0) {
      xx = xx - 1
    }
    drawCircleRec(centre_x, centre_y, xx, 0, () => 1 - radius, () => Cons((xx + centre_x, centre_y), Cons((xx + centre_x, centre_y), Cons((centre_x, xx + centre_y), Cons((centre_x, xx + centre_y), Nil())))))

}

var res = drawCircle(0,0,3).asInstanceOf[Cons[(Int,Int)]]