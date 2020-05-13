object CommandParser {
  //* Command example:
  // Circle (3, 32, 3)
  // Circle'SPACE'(3,'SPACE'32,'SPACE'3)
  def parseCommands(commandsString: String): CustomList[CanvasElement] = {
    val boundingBoxCommand = commandsString
      .split("\n")(0)
    if (!boundingBoxCommand.contains("Bounding-Box")) {
      throw new Exception("Bounding-Box not declared as first command")
    }
    val splittedCommand = boundingBoxCommand
      .replace("Bounding-Box ", "")
      .replace("(", "")
      .replace(")", "")
      .split(',').toList
    val boundaries = splittedCommand.map(str => str.replace(" ", "").toInt)
    val boundary = new Boundary(boundaries(0), boundaries(1), boundaries(2), boundaries(3))

    CustomList.fromScalaList(commandsString.replace("(", "").replace(")", "")
      .split("\n").toList)
      .map(commandStr => mapToCanvasElement(commandStr, boundary))
  }

  def exceedsBoundary(boundary: Boundary, coordinate: Coordinate): Boolean = {
    if (coordinate.x > boundary.x0 && coordinate.x < boundary.x1 &&
      coordinate.y > boundary.y0 && coordinate.y < boundary.y1)
      return true

    false
  }

  def createText(value: CustomList[String]): CanvasElement = {
    val list = value.map(str => str.replace(" ", ""))
    val coord = new Coordinate(list(0).toInt, list(1).toInt)
    val coordinates = Cons(coord, Nil())

    new TextAt(coordinates, list(2))

  }

  def mapToCanvasElement(command: String, boundary: Boundary): CanvasElement = command.split(' ')(0) match {
    case "Bounding-Box" => createBoundingBox(CustomList.fromScalaList(command.replace("Bounding-Box", "").split(',').toList))
    case "Circle" => createCircle(CustomList.fromScalaList(command.replace("Circle", "").split(',').toList), boundary)
    case "Line" => createLine(CustomList.fromScalaList(command.replace("Line", "").split(',').toList), boundary)
    case "Rectangle" => createRectangle(CustomList.fromScalaList(command.replace("Rectangle", "").split(',').toList), boundary)
    case "Text-At" => createText(CustomList.fromScalaList(command.replace("Text-At", "").split(',').toList))
    case "Fill" => createFillOfObject(CustomList.fromScalaList(command.replace("Fill", "").split(',').toList), boundary)
    case "Pie-Chart" => createPieChart(CustomList.fromScalaList(command.replace("Pie-Chart", "").split(',').toList),boundary)
    case "Draw" => createDraw(CustomList.fromScalaList(command.replace("Draw", "").split(";").toList),boundary)
    case _ => throw new Exception("Unknown command: " + command)
  }

  def createDraw(listOfParams: CustomList[String], boundary: Boundary): DrawObjects = {
    val color = listOfParams.asInstanceOf[Cons[String]].head.replace(" ", "")
    val res = listOfParams.asInstanceOf[Cons[String]].tail.map(x => mapToCanvasElement(x,boundary))

    new DrawObjects(color,res,Nil())
  }


  def createFillOfObject(listOfParams: CustomList[String], boundary: Boundary): Fill = {
    val color = listOfParams.asInstanceOf[Cons[String]].head.replace(" ", "")
    val objectToFill = listOfParams.asInstanceOf[Cons[String]].tail
    val objectToFillAsString = toStringList(objectToFill, "")

    val canvas = mapToCanvasElement(objectToFillAsString, boundary)

    val typeOfCanvas = canvas.getClass.getName

    typeOfCanvas match {
      case "Rectangle" =>
        fillRectangle(objectToFillAsString,canvas,color,boundary)
      case "Circle" =>
        fillCircle(objectToFillAsString,canvas,color,boundary)
      case _ => throw new Exception("Not supported shape" + typeOfCanvas)
    }
  }

  private def fillRectangle(objectToFillAsString: String, canvas : CanvasElement, color : String, boundary: Boundary): Fill = {
    val listOfParams = objectToFillAsString.replace("Rectangle", "").split(',').toList
    val list = listOfParams.map(str => str.replace(" ", "").toInt)
    val seed_x = Math.round((list(0) + list(2)) / 2)
    val seed_y = Math.round((list(1) + list(3)) / 2)
    val res = CustomList.filter(Draw.fillObject(seed_x, seed_y, canvas.coordinates, Nil()), coordinate => exceedsBoundary(boundary, coordinate))
    return new Fill(res, color, canvas)
  }

  private def fillCircle(objectToFillAsString: String, canvas: CanvasElement, color: String,boundary: Boundary): Fill = {
    val listOfParams = objectToFillAsString.replace("Circle", "").split(',').toList
    val list = listOfParams.map(str => str.replace(" ", "").toInt)
    val seed_x = list(0)
    val seed_y = list(1)
    val res = CustomList.filter(Draw.fillObject(seed_x, seed_y, canvas.coordinates, Nil()), coordinate => exceedsBoundary(boundary, coordinate))
    return new Fill(res, color, canvas)
  }

  @scala.annotation.tailrec
  private def toStringList(listOfString: CustomList[String], str: String): String = listOfString match {
    case Nil() => str;
    case Cons(head: String, tail: CustomList[String]) => toStringList(tail, str.concat(head+","))
  }

  def createCircle(listOfParams: CustomList[String], boundary: Boundary): CanvasElement = {
    val list = listOfParams.map(str => str.replace(" ", "").toInt)
    val coordinates = CustomList.filter(Draw.drawCircle(list(0), list(1), list(2)), coordinate => exceedsBoundary(boundary, coordinate))

    new Circle(coordinates)
  }

  def createLine(listOfParams: CustomList[String], boundary: Boundary): CanvasElement = {
    val list = listOfParams.map(str => str.replace(" ", "").toInt)
    val coordinates = CustomList.filter(Draw.drawLine(list(0), list(1), list(2), list(3)), coordinate => exceedsBoundary(boundary, coordinate))
    new Line(coordinates)
  }

  def createRectangle(listOfParams: CustomList[String], boundary: Boundary): CanvasElement = {
    val list = listOfParams.map(str => str.replace(" ", "").toInt)
    val coordinates = CustomList.filter(Draw.drawRectangle(list(0), list(1), list(2), list(3)), coordinate => exceedsBoundary(boundary, coordinate))
    new Rectangle(coordinates)
  }

  def createBoundingBox(listOfParams: CustomList[String]): CanvasElement = {
    val list = listOfParams.map(str => str.replace(" ", "").toInt)
    val coordinates = Draw.drawRectangle(list(0), list(1), list(2), list(3))
    new BoundingBox(coordinates)
  }

  def createPieChart(listOfParams: CustomList[String], boundary: Boundary): CanvasElement = {
    val params = listOfParams.map(str => str.replace(" ", "").toInt)
    val radius = params(0)
    val centre_x = params(1)
    val centre_y = params(2)
    val slices = params.skip(3)

    val coords = CustomList.filter(mapToLines(centre_x, centre_y, 0, radius, slices, () => Nil[CustomList[Coordinate]]()).reduce(Nil[Coordinate](), (a: CustomList[Coordinate], b: CustomList[Coordinate]) => a.merge(b)).merge(Draw.drawCircle(centre_x, centre_y, radius)),coordinate => exceedsBoundary(boundary,coordinate))

    new Circle(coords)
  }

  private def lineParams = (x: Int, y: Int, x1: Int, y1: Int) => Math.abs(x1 - x) >= Math.abs(y1 - y) && x1 < x || Math.abs(x1 - x) < Math.abs(y1 - y) && y1 < y match {
    case true => (x1, y1, x, y)
    case false => (x, y, x1, y1)
  }

  private def endCoordinates = (x: Int, y: Int, percent: Int, radius: Int) => {
    val deg = percent * 360.0 / 100
    val rad = Math.toRadians(deg)
    val x1 = (x + radius * Math.cos(rad)).toInt
    val y1 = (y + radius * Math.sin(rad)).toInt

    (x1,y1)
  }

  @scala.annotation.tailrec
  private def mapToLines(x: Int, y: Int, percent: Int, radius: Int, slices: CustomList[Int], listBuilder: () => CustomList[CustomList[Coordinate]]): CustomList[CustomList[Coordinate]] = slices match {
    case Nil() => listBuilder()
    case Cons(head, tail) =>
      val endCoords = endCoordinates(x, y, percent + head, radius)
      val coords = lineParams(x, y, endCoords._1, endCoords._2)
      mapToLines(x, y, percent + head, radius, tail, () => Cons(Draw.drawLine(coords._1, coords._2, coords._3, coords._4), listBuilder()))
  }
}

class Coordinate(val x: Int, val y: Int) {
  override def equals(obj: Any): Boolean = obj match {
    case o: Coordinate => o.x == x && o.y == y
    case _ => false
  }
}

abstract class CanvasElement(val coordinates: CustomList[Coordinate])

class Circle(coordinates: CustomList[Coordinate]) extends CanvasElement(coordinates)

class Rectangle(coordinates: CustomList[Coordinate]) extends CanvasElement(coordinates)

class BoundingBox(coordinates: CustomList[Coordinate]) extends CanvasElement(coordinates)

class Line(coordinates: CustomList[Coordinate]) extends CanvasElement(coordinates)

class TextAt(coordinates: CustomList[Coordinate], var text: String) extends CanvasElement(coordinates)

class Fill(val coordinatesToBeColored: CustomList[Coordinate], val color: String, val elementToBeFilled: CanvasElement) extends CanvasElement(coordinatesToBeColored)

class DrawObjects(val color: String, val elements: CustomList[CanvasElement], coordinates: CustomList[Coordinate]) extends CanvasElement(coordinates)

class Boundary(var x0: Int, var y0: Int, var x1: Int, var y1: Int)

