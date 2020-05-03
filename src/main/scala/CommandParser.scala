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
    return new TextAt(false, Color.BLACK, coordinates, list(2))
  }

  def mapToCanvasElement(command: String, boundary: Boundary): CanvasElement = command.split(' ')(0) match {
    case "Bounding-Box" => createBoundingBox(CustomList.fromScalaList(command.replace("Bounding-Box ", "").split(',').toList))
    case "Circle" => createCircle(CustomList.fromScalaList(command.replace("Circle ", "").split(',').toList), boundary)
    case "Line" => createLine(CustomList.fromScalaList(command.replace("Line ", "").split(',').toList), boundary)
    case "Rectangle" => createRectangle(CustomList.fromScalaList(command.replace("Rectangle ", "").split(',').toList), boundary)
    case "Text-At" => createText(CustomList.fromScalaList(command.replace("Text-At ", "").split(',').toList))
    case _ => throw new Exception("Unknown command: " + command)
  }

  def createCircle(listOfParams: CustomList[String], boundary: Boundary) : CanvasElement = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    var coordinates = CustomList.filter(draw.drawCircle(list(0), list(1), list(2)), coordinate => exceedsBoundary(boundary, coordinate))
    return new Circle(false, Color.BLACK, coordinates)
  }

  def createLine(listOfParams: CustomList[String], boundary: Boundary) : CanvasElement = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    var coordinates = CustomList.filter(draw.drawLine(list(0), list(1), list(2), list(3)), coordinate => exceedsBoundary(boundary, coordinate))
    return new Line(false, Color.BLACK, coordinates)
  }

  def createRectangle(listOfParams: CustomList[String], boundary: Boundary) : CanvasElement = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    var coordinates = CustomList.filter(draw.drawRectangle(list(0), list(1), list(2), list(3)), coordinate => exceedsBoundary(boundary, coordinate))
    return new Rectangle(false, Color.BLACK, coordinates)
  }

  def createBoundingBox(listOfParams: CustomList[String]) : CanvasElement = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    var coordinates = draw.drawRectangle(list(0), list(1), list(2), list(3))
    return new BoundingBox(false, Color.YELLOW, coordinates)
  }
}

class Coordinate(var x : Int, var y : Int) {
}

abstract class CanvasElement(fill: Boolean, color: Color, val coordinates: CustomList[Coordinate])

class Circle(var fill: Boolean, var color: Color, coordinates: CustomList[Coordinate]) extends CanvasElement(fill, color, coordinates)
class Rectangle(var fill: Boolean, var color: Color, coordinates: CustomList[Coordinate]) extends CanvasElement(fill, color, coordinates)
class BoundingBox(var fill: Boolean, var color: Color, coordinates: CustomList[Coordinate]) extends CanvasElement(fill, color, coordinates)
class Line(var fill: Boolean, var color: Color, coordinates: CustomList[Coordinate]) extends CanvasElement(fill, color, coordinates)
class TextAt(var fill: Boolean, var color: Color, coordinates: CustomList[Coordinate], var text: String) extends CanvasElement(fill, color, coordinates)

class Boundary(var x0: Int, var y0: Int, var x1: Int, var y1: Int)
