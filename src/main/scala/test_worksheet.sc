sealed abstract class CustomList[T];

case class Nil[T]() extends CustomList[T]
case class Cons[T](head: T, tail: CustomList[T]) extends CustomList[T]

object CustomList {
  def append[T](list: CustomList[T], element: T): CustomList[T] = Cons(element, list);

  def appendIf[T](list: CustomList[T], element: T, predicate: () => Boolean): CustomList[T] = predicate() match {
    case true => append(list, element);
    case false => list;
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

def drawCircleRec(centre_x: Int, centre_y: Int, radius: Int, x: Int, y: Int, p: () => Int, coords: CustomList[(Int,Int)]): CustomList[(Int,Int)] = {
  if(x < y) {
    return coords;
  }

  var coordinates: CustomList[(Int,Int)] = coords;

  var newX = if (p() <= 0) x else x - 1;
  coordinates = CustomList.merge(coordinates, Cons((newX+ centre_x, y + centre_y), Cons((-newX + centre_x, y + centre_y), Cons((newX + centre_x, -y + centre_y), Cons((-newX + centre_x, -y + centre_y), Nil())))));

  if(newX != y) {
    coordinates = CustomList.merge(coordinates, Cons((y + centre_x, newX + centre_y), Cons((-y + centre_x, newX + centre_y), Cons((y + centre_x, -newX + centre_y), Cons((-y + centre_x, -newX + centre_y), Nil())))))
  }

  if(p() <= 0) {
    drawCircleRec(centre_x, centre_y, radius, newX, y+1, () => p() + 2*y + 1, coordinates)
  } else {
    drawCircleRec(centre_x, centre_y, radius, newX, y+1, () => p() + 2*y - 2*newX + 1, coordinates)
  }
}

def drawCircle(centre_x: Int, centre_y: Int, radius: Int): CustomList[(Int,Int)] = {
  var x = radius;
  var y = 0;

  var cont: CustomList[(Int,Int)] = Nil[(Int,Int)]();

  cont = CustomList.append(cont, (x + centre_x, y + centre_y));

  cont = CustomList.appendIf(cont, (x + centre_x, -y + centre_y), () => radius > 0);
  cont = CustomList.appendIf(cont, (y + centre_x, x + centre_y), () => radius > 0);
  cont = CustomList.appendIf(cont, (-y + centre_x, x + centre_y), () => radius > 0);


  return drawCircleRec(centre_x, centre_y, radius, x, y+1, () => 1 - radius, cont);
}

def drawCircleNonRec(centre_x: Int, centre_y: Int, radius: Int): Unit = {
  var x = radius;
  var y = 0;

  println(s"${x + centre_x}, ${y + centre_y}");

  if(radius > 0) {
    println(s"${x + centre_x}, ${-y + centre_y}");
    println(s"${y + centre_x}, ${x + centre_y}");
    println(s"${-y + centre_x}, ${x + centre_y}");
  }

  var p = 1 - radius;

  while(x > y) {
    y = y + 1;

    if(p <= 0) {
      p = p + 2*y + 1;
    } else {
      x = x - 1;
      p = p + 2*y - 2*x + 1;
    }

    if(x < y) {
      return;
    }

    println(s"${x + centre_x}, ${y + centre_y}");
    println(s"${-x + centre_x}, ${y + centre_y}");
    println(s"${x + centre_x}, ${-y + centre_y}");
    println(s"${-x + centre_x}, ${-y + centre_y}");

    if(x != y) {
      println(s"${y + centre_x}, ${x + centre_y}");
      println(s"${-y + centre_x}, ${x + centre_y}");
      println(s"${y + centre_x}, ${-x + centre_y}");
      println(s"${-y + centre_x}, ${-x + centre_y}");
    }
  }
}

val test = d(0,0,3);
printList(test)
//val list1 = Cons(1, Cons(2, Cons(3, Nil())));
//val list2 = Cons(4, Cons(5, Cons(6, Nil())));
//
//val merged = CustomList.merge(list1, list2);
//printList(merged)
println("nonrec:");
//drawCircleNonRec(0,0,3)
//var res = drawCircle(0,0,3).asInstanceOf[Cons[(Int,Int)]]