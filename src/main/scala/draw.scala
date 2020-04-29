object draw {
  def drawLine(x0: Int, y0: Int, x1: Int, y1: Int): CustomList[(Int, Int)] = {
    return CustomList.fromScalaList((x0 to x1).map(x => (x,bresenhams(x0, y0, x1, y1, x))).toList)
  }

  def bresenhams(x0: Int, y0: Int, x1: Int, y1: Int, x: Int): Int = {
    return ((y1 - y0) / (x1 - x0))*(x-x0) + y0
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

    var coords: CustomList[(Int,Int)] = Nil[(Int,Int)]();
//    coords.append((x + centre_x, y + centre_y)).appendIf((x + centre_x, -y + centre_y), () => radius > 0).appendIf((y + centre_x, x + centre_y), () => radius > 0).appendIf((-y + centre_x, x + centre_y), () => radius > 0)
    coords = CustomList.append(coords, (x + centre_x, y + centre_y));

    coords = CustomList.appendIf(coords, (x + centre_x, -y + centre_y), () => radius > 0);
    coords = CustomList.appendIf(coords, (y + centre_x, x + centre_y), () => radius > 0);
    coords = CustomList.appendIf(coords, (-y + centre_x, x + centre_y), () => radius > 0);


    return drawCircleRec(centre_x, centre_y, radius, x, y+1, () => 1 - radius, coords);
  }
}
