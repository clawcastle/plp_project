import java.awt.Color

object CommandParser {
  //* Command example:
  // Circle (3, 32, 3)
  // Circle'SPACE'(3,'SPACE'32,'SPACE'3)
  def parseCommands(commandsString: String): CustomList[CanvasElement] = {
    var boundingBoxCommand = commandsString
    .split("\n")(0)
    if (!boundingBoxCommand.contains("Bounding-Box")) {
      throw new Exception("Bounding-Box not declared as first command")
    }
    var splittedCommand = boundingBoxCommand
      .replace("Bounding-Box ", "")
      .replace("(", "")
      .replace(")", "")
      .split(',').toList
    var boundaries = splittedCommand.map(str => str.replace(" ", "").toInt)
    var boundary = new Boundary(boundaries(0), boundaries(1), boundaries(2), boundaries(3))

    return CustomList.fromScalaList(commandsString.replace("(", "").replace(")", "")
      .split("\n").toList)
      .map(commandStr => mapToCanvasElement(commandStr, boundary))
  }

  def exceedsBoundary(boundary: Boundary, coordinate: Coordinate): Boolean = {
    if (coordinate.x > boundary.x0 && coordinate.x < boundary.x1 &&
      coordinate.y > boundary.y0 && coordinate.y < boundary.y1)
      return true;

    return false;
  }

  def createText(value: CustomList[String]): CanvasElement = {
    var list = value.map(str => str.replace(" ", ""))
    var coord = new Coordinate(list(0).toInt, list(1).toInt)
    var coordinates = Cons(coord, Nil())

    return new TextAt(coordinates, list(2))

  }

  def mapToCanvasElement(command: String, boundary: Boundary): CanvasElement = command.split(' ')(0) match {
    case "Bounding-Box" => createBoundingBox(CustomList.fromScalaList(command.replace("Bounding-Box ", "").split(',').toList))
    case "Circle" => createCircle(CustomList.fromScalaList(command.replace("Circle ", "").split(',').toList), boundary)
    case "Line" => createLine(CustomList.fromScalaList(command.replace("Line ", "").split(',').toList), boundary)
    case "Rectangle" => createRectangle(CustomList.fromScalaList(command.replace("Rectangle ", "").split(',').toList), boundary)
    case "Text-At" => createText(CustomList.fromScalaList(command.replace("Text-At ", "").split(',').toList))
    case "Pie-Chart" => pieChart()
    case _ => throw new Exception("Unknown command: " + command)
  }

  def createCircle(listOfParams: CustomList[String], boundary: Boundary) : CanvasElement = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    var coordinates = CustomList.filter(draw.drawCircle(list(0), list(1), list(2)), coordinate => exceedsBoundary(boundary, coordinate))

    return new Circle(coordinates)
  }

  def createLine(listOfParams: CustomList[String], boundary: Boundary) : CanvasElement = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    var coordinates = CustomList.filter(draw.drawLine(list(0), list(1), list(2), list(3)), coordinate => exceedsBoundary(boundary, coordinate))
    return new Line(coordinates)
  }

  def createRectangle(listOfParams: CustomList[String], boundary: Boundary) : CanvasElement = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    var coordinates = CustomList.filter(draw.drawRectangle(list(0), list(1), list(2), list(3)), coordinate => exceedsBoundary(boundary, coordinate))
    return new Rectangle(coordinates)
  }

  def createBoundingBox(listOfParams: CustomList[String]) : CanvasElement = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    var coordinates = draw.drawRectangle(list(0), list(1), list(2), list(3))
    return new BoundingBox(coordinates)
  }

  def lineParams = (x: Int, y: Int, x1: Int, y1: Int) => ((Math.abs(x1 - 200) >= Math.abs(y1 - 200) && x1 < 200) || (Math.abs(x1 - 200) < Math.abs(y1 - 200) && y1 < 200)) match {
    case true => (x1, y1, x, y)
    case false => (x, y, x1, y1)
  }

  def endCoordinates = (percent: Int, radius: Int) => {
    val deg = percent * (360.0 / 100);
    val rad = Math.toRadians(deg)
    val x1 = (200 + radius * Math.cos(rad)).toInt
    val y1 = (200 + radius * Math.sin(rad)).toInt

    (x1,y1)
  }

  def mapToLines(x: Int, radius: Int, slices: CustomList[Int], listBuilder: () => CustomList[CustomList[Coordinate]]): CustomList[CustomList[Coordinate]] = slices match {
    case Nil() => listBuilder()
    case Cons(head, tail) => {
      val endCoords = endCoordinates(x + head, radius)
      val coords = lineParams(200, 200, endCoords._1, endCoords._2)
      val t = mapToLines(x + head, radius, tail, () => Cons(draw.drawLine(coords._1, coords._2, coords._3, coords._4), listBuilder()))
      return t
    }
  }

  def pieChart(radius: Int = 100): CanvasElement = {
    val slices = Cons(9, CustomList.range(1,13))

    val coords = mapToLines(0, radius, slices, () => Nil[CustomList[Coordinate]]()).reduce(Nil[Coordinate](), (a: CustomList[Coordinate], b: CustomList[Coordinate]) => a.merge(b)).merge(draw.drawCircle(200, 200, radius))


    return new Circle(coords);
  }
}

class Coordinate(var x : Int, var y : Int) {
}

abstract class CanvasElement(val coordinates: CustomList[Coordinate])

class Circle(coordinates: CustomList[Coordinate]) extends CanvasElement(coordinates)
class Rectangle(coordinates: CustomList[Coordinate]) extends CanvasElement(coordinates)
class BoundingBox(coordinates: CustomList[Coordinate]) extends CanvasElement(coordinates)
class Line(coordinates: CustomList[Coordinate]) extends CanvasElement(coordinates)
class TextAt(coordinates: CustomList[Coordinate], var text: String) extends CanvasElement(coordinates)
class Fill(coordinatesToBeColored: CustomList[Coordinate], color: Color, elementToBeFilled : CanvasElement) extends CanvasElement(coordinatesToBeColored)

class Boundary(var x0: Int, var y0: Int, var x1: Int, var y1: Int)

