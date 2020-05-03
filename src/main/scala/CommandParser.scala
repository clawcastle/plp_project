import java.awt.Color
import java.lang.reflect.Field

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
    case "Fill" => createFillOfObject(CustomList.fromScalaList(command.replace("Fill ", "").split(',').toList), boundary)
    case _ => throw new Exception("Unknown command: " + command)
  }

  def createFillOfObject(listOfParams: CustomList[String], boundary: Boundary): Fill = {
    var color = listOfParams.asInstanceOf[Cons[String]].head
    var objectToFill = listOfParams.asInstanceOf[Cons[String]].tail
    var objectToFillAsString = toStringList(objectToFill, "")
    var canvas = mapToCanvasElement(objectToFillAsString, boundary)

    var seed_x: Int = 0
    var seed_y: Int = 0
    val typeOfCanvas = canvas.getClass().getName()
    typeOfCanvas match {
      case "Rectangle" =>
        val listOfParams = objectToFillAsString.replace("Rectangle ", "").split(',').toList
        val list = listOfParams.map(str => str.replace(" ", "").toInt)
        var average = (list(0)+list(2))/2
        seed_x = Math.round((list(0)+list(2))/2)
        seed_y = Math.round((list(1)+list(3))/2)
      case "Circle" =>
        val listOfParams = objectToFillAsString.replace("Circle ", "").split(',').toList
        val list = listOfParams.map(str => str.replace(" ", "").toInt)
        seed_x = list(0)
        seed_y = list(1)
      case _ => throw new Exception("Not supported shape"+typeOfCanvas)
    }

    var res = CustomList.filter(draw.fillObject(seed_x, seed_y, color, canvas.coordinates, Nil()),coordinate => exceedsBoundary(boundary,coordinate))

    return new Fill(res,Color.getColor(color),canvas)

  }

  private def toStringList(listOfString: CustomList[String], str: String): String = listOfString match {
    case Nil() => str;
    case Cons(head: String, tail: CustomList[String]) => toStringList(tail, str.concat(head+","))
  }

  def createCircle(listOfParams: CustomList[String], boundary: Boundary): CanvasElement = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    var coordinates = CustomList.filter(draw.drawCircle(list(0), list(1), list(2)), coordinate => exceedsBoundary(boundary, coordinate))
    return new Circle(coordinates)
  }

  def createLine(listOfParams: CustomList[String], boundary: Boundary): CanvasElement = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    var coordinates = CustomList.filter(draw.drawLine(list(0), list(1), list(2), list(3)), coordinate => exceedsBoundary(boundary, coordinate))
    return new Line(coordinates)
  }

  def createRectangle(listOfParams: CustomList[String], boundary: Boundary): CanvasElement = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    var coordinates = CustomList.filter(draw.drawRectangle(list(0), list(1), list(2), list(3)), coordinate => exceedsBoundary(boundary, coordinate))
    return new Rectangle(coordinates)
  }

  def createBoundingBox(listOfParams: CustomList[String]): CanvasElement = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    var coordinates = draw.drawRectangle(list(0), list(1), list(2), list(3))
    return new BoundingBox(coordinates)
  }
}

class Coordinate(var x: Int, var y: Int) {
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

class Fill(val coordinatesToBeColored: CustomList[Coordinate], val color: Color, val elementToBeFilled: CanvasElement) extends CanvasElement(coordinatesToBeColored)

class Boundary(var x0: Int, var y0: Int, var x1: Int, var y1: Int)

