object Json {
  class Parser {
    private var s: String = ""
    private var currentPos: Int = 0

    def parse(str: String): Object = {
      s = str
      currentPos = 0
      val result = nextValue()
      if (currentPos != s.length)
        throw exception(s.substring(currentPos), currentPos, "end of input")
      result
    }

    private def exception(token: String, at: Int, expected: String) = {
      val subS = s.substring(0, at)
      val row = "\\n".r.findAllMatchIn(subS).size + 1
      val col = at - (if (subS.lastIndexOf('\n') == -1) 0 else subS.lastIndexOf('\n'))
      new IllegalArgumentException(s"unexpected token $token at $at (row: $row, col: $col), expected: $expected")
    }

    private val numberChars = Set('-', '+', '.', 'e', 'E', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
    private val validNumber = """^-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?$""".r

    private def nextToken() = {
      def skipEmpty() = while (s.charAt(currentPos) == ' ' || s.charAt(currentPos) == '\t' ||
          s.charAt(currentPos) == '\r' || s.charAt(currentPos) == '\n')
        currentPos += 1

      skipEmpty()
      val token = s.charAt(currentPos) match {
        case c @ ('{' | '}' | '[' | ']' | ',' | ':') => c.toString
        case 'n' if s.substring(currentPos, currentPos + 4) == "null" => "null"
        case 't' if s.substring(currentPos, currentPos + 4) == "true" => "true"
        case 'f' if s.substring(currentPos, currentPos + 5) == "false" => "false"
        case '"' =>
          def isCharEscapedAt(j: Int) = {
            var k = j
            while (s.charAt(k - 1) == '\\')
              k -= 1
            val backslashes = j - k
            backslashes % 2 != 0
          }

          var j = currentPos + 1
          while (s.charAt(j) != '"' || isCharEscapedAt(j))
            j += 1
          s.substring(currentPos, j + 1)
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          var j = currentPos + 1
          while (j < s.length && numberChars.contains(s.charAt(j)))
            j += 1
          val result = s.substring(currentPos, j)
          if (validNumber.findFirstIn(result).isEmpty)
            throw exception(result, currentPos, "json number")
          result
        case char => char.toString
      }
      currentPos += token.length
      token
    }

    private def nextValue(): Object = nextToken() match {
      case "{" => fillMap()
      case "[" => fillArray()
      case "null" => null
      case "true" => java.lang.Boolean.TRUE
      case "false" => java.lang.Boolean.FALSE
      case str: String => (str.head match {
        case '"' => str.substring(1, str.length - 1)
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => java.lang.Double.parseDouble(str)
        case t => throw exception(t.toString, currentPos - str.length, "json value")
      }).asInstanceOf[Object]
    }

    private def nextField() = {
      val key = nextToken()
      if (key.head != '"')
        throw exception(key, currentPos - key.length, "\"")
      skip(":")
      val value = nextValue()
      (key.substring(1, key.length - 1), value)
    }

    private def skip(t: String) = {
      val result = nextToken()
      if (result != t)
        throw exception(result, currentPos - result.length, t)
    }

    private def fillArray() = {
      val a = new java.util.ArrayList[Object]
      fillComposite(a.add(nextValue()), "]")
      a
    }

    private def fillMap() = {
      val m = new java.util.HashMap[String, Object]
      fillComposite({
        val (key, value) = nextField()
        m.put(key, value)
      }, "}")
      m
    }

    private def fillComposite[T](doFill: => Unit, end: String) = {
      var t = nextToken()
      if (t != end) {
        currentPos -= t.length
        while (t != end) {
          doFill
          t = nextToken()
          if (t != "," && t != end)
            throw exception(t, currentPos - t.length, s", or $end")
        }
      }
    }

  }

  sealed trait Wrapper {
    def parsed: Any

    def /(pathSegment: String): Wrapper

    def isEmpty: Boolean

    def nonEmpty: Boolean

    def isNull: Boolean

    def asString: Option[String]

    def asBoolean: Option[Boolean]

    def asDouble: Option[Double]

    def asArray: Option[IndexedSeq[Any]]

    def asMap: Option[Map[String, Any]]

    def asWrappedArray: Option[IndexedSeq[Wrapper]]

    def asWrappedMap: Option[Map[String, Wrapper]]
  }

  object Wrapper {
    def apply(parsed: Any): Wrapper = new WrapperSome(parsed)
  }

  object WrapperNone extends Wrapper {
    override def parsed = null

    override def /(pathSegment: String) = WrapperNone

    override def isEmpty = true

    override def nonEmpty = false

    override def isNull = false

    override def asString = None

    override def asBoolean = None

    override def asDouble = None

    override def asArray = None

    override def asMap = None

    override def asWrappedArray = None

    override def asWrappedMap = None
  }

  class WrapperSome(val parsed: Any) extends Wrapper {
    import scala.collection.JavaConverters._

    def /(pathSegment: String) = parsed match {
      case m: java.util.Map[String, Object] if m.containsKey(pathSegment) => new WrapperSome(m.get(pathSegment))
      case _ => WrapperNone
    }

    override def isEmpty = false

    override def nonEmpty = true

    override def isNull = parsed == null

    override def asString = parsed match {
      case x: String => Some(x)
      case _ => None
    }

    override def asBoolean = parsed match {
      case x: Boolean => Some(x)
      case _ => None
    }

    override def asDouble = parsed match {
      case x: Double => Some(x)
      case _ => None
    }

    override def asArray = parsed match {
      case x: java.util.List[Object] => Some(x.asScala.toIndexedSeq)
      case _ => None
    }

    override def asMap = parsed match {
      case x: java.util.Map[String, Object] => Some(Map(x.asScala.toSeq: _*))
      case _ => None
    }

    override def asWrappedArray = asArray.map(_.map(Wrapper.apply))

    override def asWrappedMap = asMap.map(_.mapValues(Wrapper.apply))
  }
}
