object commandParser {
  def parseCommand(commandsString: String): List[(Int,Int)] = {
    splitCommands(commandsString).map(str => replaceParentheses(str)).map(str => mapToHandler(str)(str))
  }

  def splitCommands(inputString: String): List[String] = {
    inputString.split(sys.props("line.separator")).toList
  }

  def replaceParentheses(inputString: String): String = {
    inputString.replace("(", "").replace(")", "")
  }

  def mapToHandler(commandString: String): String => List[(Int,Int)] = {
    val shape = commandString.split(" ")(0)

    shape match {
      case "line" => {
        return str => drawLine(str)
      }
    }
  }

  def drawLine(command: String): List[(Int,Int)] = {
    val params = command.split(" ").tail.map(x => x.toInt)
    draw.drawLine(params(0), params(1), params(2), params(3))
  }

  def transformString(str: String, splitChar: Char, transformers: List[String => String]): List[String] = {
    transformers match {
      case Nil => str.split(splitChar).toList;
      case t => transformString(t.head(str), splitChar, t.tail)
    }
  }

  def parseCommands(commandsString: String): List[List[String]] = {
    commandsString.split(sys.props("line.separator")).map(str => transformString(str, ' ', List(replaceParentheses))).toList
  }

  def mapToShapes(commands: String): Unit = {
    parseCommands(commands).map(l => draw.drawLine(l))
  }
}

class Shape {
  val coordinates: List[Coordinates] = List[Coordinates]()
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