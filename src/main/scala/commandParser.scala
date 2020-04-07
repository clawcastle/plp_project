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
}