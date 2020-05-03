object CommandParser {
  //* Command example:
  // Circle (3, 32, 3)
  // Circle'SPACE'(3,'SPACE'32,'SPACE'3)
  def parseCommands(commandsString: String): CustomList[CustomList[Coordinate]] = {
    return CustomList.fromScalaList(commandsString.replace("(", "").replace(")", "")
      .split("\n").toList)
      .map(commandStr => mapToShapes(commandStr)).map(shape => filterBoundingBox(commandsString.split("\n")(0), shape))
  }

  def filterBoundingBox(command: String, shape: CustomList[Coordinate]): CustomList[Coordinate] = {
    if (command.contains("Bounding-Box")) {
      var splittedCommand = command
        .replace("Bounding-Box ", "")
        .replace("(", "")
        .replace(")", "")
        .split(',').toList
      var boundary = splittedCommand.map(str => str.replace(" ", "").toInt)
      return CustomList.filter(shape, coordinate => exceedsBoundary(boundary.head, boundary(1), boundary(2), boundary(3), coordinate))
    }
    else throw new Exception("Bounding-Box not declared as first command")
  }

  def exceedsBoundary(x0: Int, y0: Int, x1: Int, y1: Int, coordinate: Coordinate): Boolean = {
    if (coordinate.x > x0-1 && coordinate.x < x1+1 &&
      coordinate.y > y0-1 && coordinate.y < y1+1)
      return true;

    return false;
  }

  def mapToShapes(commands: String): CustomList[Coordinate] = commands.split(' ')(0) match {
    case "Bounding-Box" => createRectangle(CustomList.fromScalaList(commands.replace("Bounding-Box ", "").split(',').toList))
    case "Circle" => createCircle(CustomList.fromScalaList(commands.replace("Circle ", "").split(',').toList))
    case "Line" => createLine(CustomList.fromScalaList(commands.replace("Line ", "").split(',').toList))
    case "Rectangle" => createRectangle(CustomList.fromScalaList(commands.replace("Rectangle ", "").split(',').toList))
    case _ => throw new Exception("Unknown shape")
  }

  def createCircle(listOfParams: CustomList[String]) : CustomList[Coordinate] = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    draw.drawCircle(list(0), list(1), list(2))
  }

  def createLine(listOfParams: CustomList[String]) : CustomList[Coordinate] = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    draw.drawLine(list(0), list(1), list(2), list(3))
  }

  def createRectangle(listOfParams: CustomList[String]) : CustomList[Coordinate] = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    draw.drawRectangle(list(0), list(1), list(2), list(3))
  }


}

class Coordinate(var x : Int, var y : Int) {
}