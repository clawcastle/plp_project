object draw {
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

  def drawLine(x0: Int, y0: Int, x1: Int, y1: Int): CustomList[Coordinate] = if (((x1 - x0).abs >= (y1 - y0).abs)) {
    CustomList.fromScalaList((x0 to x1).map(x => (x, findYBresenhams(x0, y0, x1, y1, x))).toList.map(coordinate => new Coordinate(coordinate._1, coordinate._2)))
  } else {
    CustomList.fromScalaList((y0 to y1).map(y => (findXBresenhams(x0, y0, x1, y1, y), y)).toList.map(coordinate => new Coordinate(coordinate._1, coordinate._2)))
  }

  def drawRectangle(x0: Int, y0: Int, x1: Int, y1: Int): CustomList[Coordinate] = (x0 == x1 & y0 == y1) match {
    case true => Cons((x0, y1), Nil()).map(coordinate => new Coordinate(coordinate._1, coordinate._2))
    case false => drawLine(x0, y0, x0, y1).merge(drawLine(x0, y0, x1, y0)).merge(drawLine(x1, y0, x1, y1)).merge(drawLine(x0, y1, x1, y1))
  }

  def drawCircleRec(centre_x: Int, centre_y: Int, radius: Int, x: Int, y: Int, p: () => Int, coords: CustomList[(Int, Int)]): CustomList[(Int, Int)] = {
    if (x < y) {
      return coords;
    }

    var newX = if (p() <= 0) x else x - 1;
    var coordinates: CustomList[(Int, Int)] = coords
      .merge(Cons((newX + centre_x, y + centre_y), Cons((-newX + centre_x, y + centre_y), Cons((newX + centre_x, -y + centre_y), Cons((-newX + centre_x, -y + centre_y), Nil())))))
      .mergeIf(Cons((y + centre_x, newX + centre_y), Cons((-y + centre_x, newX + centre_y), Cons((y + centre_x, -newX + centre_y), Cons((-y + centre_x, -newX + centre_y), Nil())))), () => newX != y)

    if (p() <= 0) {
      drawCircleRec(centre_x, centre_y, radius, newX, y + 1, () => p() + 2 * y + 1, coordinates)
    } else {
      drawCircleRec(centre_x, centre_y, radius, newX, y + 1, () => p() + 2 * y - 2 * newX + 1, coordinates)
    }
  }

  def drawCircle(centre_x: Int, centre_y: Int, radius: Int): CustomList[Coordinate] = {
    var x = radius;
    var y = 0;

    var coords: CustomList[(Int, Int)] = Cons((x + centre_x, y + centre_y), Nil())
      .appendIf((x + centre_x, -y + centre_y), () => radius > 0)
      .appendIf((y + centre_x, x + centre_y), () => radius > 0)
      .appendIf((-y + centre_x, x + centre_y), () => radius > 0)

    return drawCircleRec(centre_x, centre_y, radius, x, y + 1, () => 1 - radius, coords).map(coordinate => new Coordinate(coordinate._1, coordinate._2));
  }

  def printList[T](list: CustomList[Coordinate]): Unit = {
    if(list.isInstanceOf[Nil[T]]) {
      return;
    }
    println("list")
    var l = list.asInstanceOf[Cons[T]];
    while(!l.tail.isInstanceOf[Nil[T]]) {
      println("coordinate "+l.head.asInstanceOf[Coordinate].x+", "+l.head.asInstanceOf[Coordinate].y)
      l = l.tail.asInstanceOf[Cons[T]]
    }
    println("coordinate "+l.head.asInstanceOf[Coordinate].x+", "+l.head.asInstanceOf[Coordinate].y)

  }

  def fillObject(seed_x: Int, seed_y: Int, color: String, objectCoords: CustomList[Coordinate], fillCoords: CustomList[Coordinate]): CustomList[Coordinate] = {
    //printList(objectCoords)
    println(seed_x+", "+seed_y)
    if(objectCoords.find(new Coordinate(seed_x, seed_y)) || fillCoords.find(new Coordinate(seed_x, seed_y)))
      return fillCoords;

    return fillObject(seed_x, seed_y + 1, color, objectCoords, fillObject(seed_x + 1, seed_y, color, objectCoords, fillObject(seed_x, seed_y - 1, color, objectCoords, fillObject(seed_x - 1, seed_y, color, objectCoords, fillCoords.append(new Coordinate(seed_x, seed_y))))));
  }
}
