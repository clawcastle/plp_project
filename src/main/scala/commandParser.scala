object commandParser {

  def removeCharacterFromString(str: String, charToRemove: Char): String = {
    str.replace(charToRemove.toString, "");
  }

  def drawLine(command: String): CustomList[(Int,Int)] = {
    val params = command.split(" ").tail.map(x => x.toInt)
    draw.drawLine(params(0), params(1), params(2), params(3))
  }

  def transformString(str: String, splitChar: Char, transformers: CustomList[String => String]): CustomList[String] = transformers match {
    case Nil() => CustomList.fromScalaList(str.split(splitChar).toList)
    case Cons(head, tail) => transformString(head(str), splitChar, tail)
  }

  def parseCommands(commandsString: String): CustomList[CustomList[String]] = {
    CustomList.map(CustomList.fromScalaList(commandsString.split(sys.props("line.separator")).toList), str => transformString(str, ' ', Cons(str1 => removeCharacterFromString(str1, '('), Cons(str1 => removeCharacterFromString(str1, ')'), Nil()))))
  }

  def mapToShapes(commands: String): Unit = {
    CustomList.map(parseCommands(commands), (l: CustomList[String]) => draw.drawLine(l))
  }
}

class Shape {
  val coordinates: CustomList[Coordinates] = Nil()
}

class Square extends Shape {

}

class Line extends Shape {

}

class Circle extends Shape {

}

class Coordinates {
  val x: Int = 0
  val y: Int = 0
}