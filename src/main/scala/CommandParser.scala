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
    case "Fill" => createFillOfObject(CustomList.fromScalaList(commands.replace("Fill ","").split(',').toList));
    case _ => throw new Exception("Unknown shape")
  }

  def createFillOfObject(listOfParams: CustomList[String]) : CustomList[Coordinate] = {
    var color = listOfParams.asInstanceOf[Cons[String]].head;
    var objectToFill = listOfParams.asInstanceOf[Cons[String]].tail;
    var objectCoordinates = mapToShapes(objectToFill.asInstanceOf[CustomList[String]].toString);

    draw.fillObject(100,100,color,objectCoordinates);

  }

  private def toStringList(listOfString: CustomList[String], str: String): String = {
    case Nil() => str;
    case Cons(head : String,tail : CustomList[String]) => toStringList(tail,str.concat(head))
  }

  def createCircle(listOfParams: CustomList[String]) : CustomList[Coordinate] = {
    var list = listOfParams.map(str => str.replace(" ", "").toInt)
    draw.drawCircle(list(0), list(1), list(2))
  }
}

class Coordinate(var x : Int, var y : Int) {
}