import scala.collection.immutable
sealed abstract class CustomList[T];

case class Nil[T]() extends CustomList[T]
case class Cons[T](head: T, tail: CustomList[T]) extends CustomList[T]

object CustomList {
  def append[T](list: CustomList[T], element: T): CustomList[T] = Cons(element, list);

  def appendIf[T](list: CustomList[T], element: T, predicate: () => Boolean): CustomList[T] = predicate() match {
    case true => append(list, element);
    case false => list;
  }

  def fromScalaList[T](scalaList: List[T]): CustomList[T] = scalaList match {
    case ::(head, next) => Cons(head, fromScalaList(next))
    case immutable.Nil => Nil()
  }

  def merge[T](list1: CustomList[T], list2: CustomList[T]): CustomList[T] = list1 match {
    case Nil() => list2
    case Cons(head, tail) => Cons(head, merge(tail, list2))
  }
}

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

// Kald bresenhams igen hvis y0 > y1 med ombyttede værdier

def findYBresenhams(x0: Int, y0: Int, x1: Int, y1: Int, x: Int): Int = if ((x1 == x0)) {
  y0
} else {
  ((y1 - y0) / (x1 - x0)) * (x - x0) + y0
}

def findXBresenhams(x0: Int, y0: Int, x1: Int, y1: Int, y: Int): Int = if ((y1 == y0)) {
  x0
} else {
  (x0 * y1 - x1 * y0 + (x1 - x0) * y) / (y1 - y0)
}

def drawLine(x0: Int, y0: Int, x1: Int, y1: Int): CustomList[(Int, Int)] = if (((x1 - x0).abs >= (y1 - y0).abs)) {
  CustomList.fromScalaList((x0 to x1).map(x => (x, findYBresenhams(x0, y0, x1, y1, x))).toList)
} else {
  CustomList.fromScalaList((y0 to y1).map(y => (findXBresenhams(x0, y0, x1, y1, y), y)).toList)
}


var res = printList(drawLine(0,0,0,10))

//var res = drawLine(0,0,0,10)
//var res2 = drawLine(10,0,10,10)