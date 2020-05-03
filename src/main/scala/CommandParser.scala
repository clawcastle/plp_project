object CommandParser {
  //* Command example:
  // Circle (3, 32, 3)
  // Circle'SPACE'(3,'SPACE'32,'SPACE'3)
  def parseCommands(commandsString: String): CustomList[CustomList[Coordinate]] = {
    return CustomList.fromScalaList(commandsString.replace("(", "").replace(")", "")
      .split("\n").toList)
      .map(commandStr => mapToShapes(commandStr))
  }

  def mapToShapes(commands: String): CustomList[Coordinate] = commands.split(' ')(0) match {
    case "Circle" => createCircle(CustomList.fromScalaList(commands.replace("Circle ", "").split(',').toList));
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