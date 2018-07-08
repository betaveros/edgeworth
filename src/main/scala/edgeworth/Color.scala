package edgeworth

case class Color(r: Int, g: Int, b: Int, name: Option[String]) {
  def toHexString: String = "%02x%02x%02x".format(r, g, b)
  def toStyle: String = "#%02x%02x%02x".format(r, g, b)
  def toPrettyString: String = name getOrElse toStyle
}
object Color {
  // (0, 100s, 40L, 43Lum)
  // (30, 40L, 52Lum)
  val pencil = Color(17, 17, 17, Some("Black"))
  val lightGray = Color(221, 221, 221, Some("Light Gray"))
  val azure   = Color(  0, 112, 240, Some("Azure"))
  val blue    = Color(  0,   0, 240, Some("Blue"))
  val cyan    = Color(  0, 192, 192, Some("Cyan"))
  val green   = Color(  0, 136,   0, Some("Green"))
  val magenta = Color(192,   0, 192, Some("Magenta"))
  val orange  = Color(204, 102,   0, Some("Orange"))
  val red     = Color(204,   0,   0, Some("Red"))
  val violet  = Color(112,   0, 240, Some("Violet"))
  val yellow  = Color(192, 192,   0, Some("Yellow"))

  val colorPairs = List('R' -> red, 'O' -> orange, 'Y' -> yellow, 'G' -> green, 'C' -> cyan, 'B' -> blue, 'M' -> magenta, 'A' -> azure, 'V' -> violet)
  val decodeColorMap = Map(colorPairs: _*)
  val encodeColorMap = Map((for ((s, c) <- colorPairs) yield (c, s)): _*)
  def encode(color: Color): String = if (color == Color.pencil) "" else "c" ++ encodeColorMap.get(color).map(_.toString).getOrElse(color.toHexString)
  def decode(s: StringIter): Option[Color] = if (s.peek() == Some('c')) {
    s.next() // 'c'
    try {
      decodeColorMap.get(s.peek().get) match {
        case Some(color) => { s.next(); Some(color) }
        case None => {
          val r = Integer.parseInt(s.nextN(2).get, 16)
          val g = Integer.parseInt(s.nextN(2).get, 16)
          val b = Integer.parseInt(s.nextN(2).get, 16)
          Some(Color(r, g, b, None))
        }
      }
    } catch {
      case e: NumberFormatException => {
        Out.warn("NumberFormatException while parsing color! " ++ e.toString)
        None
      }
      case e: NoSuchElementException => {
        Out.warn("Cannot parse color!")
        None
      }
    }
  } else {
    None
  }
}
