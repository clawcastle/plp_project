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

  def pieChart(radius: Int = 100): CanvasElement = {
    val slices = Cons(40, Cons(10, Cons(25, Cons(25, Nil()))));
    var coords = draw.drawCircle(200, 200, radius)
    var sum = 0;

    //CustomList.range(0, slices.length() - 1).reduce(0, (a: Int, b: CustomList[Coordinate]) => b.merge(draw.drawLine(1,2,3,4)))
    slices.forEach(slice => {
      sum = sum + slice;
      val deg = sum * (360.0 / 100);
      val rad = Math.toRadians(deg)
      val x1 = (200 + radius * Math.cos(rad)).toInt
      val y1 = (200 + radius * Math.sin(rad)).toInt

      var x_low = 200;
      var x_high = x1
      var y_low = 200;
      var y_high = y1
      if(Math.abs(x1 - 200) >= Math.abs(y1 - 200) && x1 < 200) {
        x_low = x1
        x_high = 200
        y_low = y1
        y_high = 200
      }
      var test: CustomList[Coordinate] = draw.drawLine(x_low, y_low, x_high, y_high)
      coords = coords.merge(test)
    })

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

