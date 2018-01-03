package edgeworth

case class Color(r: Int, g: Int, b: Int) {
  def toHexString: String = "%02x%02x%02x".format(r, g, b)
  def toStyle: String = "#%02x%02x%02x".format(r, g, b)
}
object Color {
  val pencil = Color(17, 17, 17)
  val lightGray = Color(221, 221, 221)
  val red = Color(204, 0, 0)
  val green = Color(0, 136, 0)
  val blue = Color(0, 0, 204)
  val colorPairs = List('R' -> red, 'G' -> green, 'B' -> blue)
  val decodeColorMap = Map(colorPairs: _*)
  val encodeColorMap = Map((for ((s, c) <- colorPairs) yield (c, s)): _*)
  def encode(color: Color): String = if (color == Color.pencil) "" else "c" ++ encodeColorMap.get(color).map(_.toString).getOrElse(color.toHexString)
  def decode(s: StringIter): Option[Color] = if (s.peek() == Some('c')) {
    s.next() // 'c'
    try {
      decodeColorMap.get(s.peek().get) match {
        case Some(color) => { s.next(); Some(color) }
        case None => {
          Some(Color(
            Integer.parseInt(s.nextN(2).get, 16),
            Integer.parseInt(s.nextN(2).get, 16),
            Integer.parseInt(s.nextN(2).get, 16)
          ))
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
