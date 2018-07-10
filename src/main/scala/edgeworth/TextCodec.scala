package edgeworth

object TextCodec {
  // This is not the most type-safe way to do this
  def directEncodeable(c: Char): Boolean = {
    ('0' <= c && c <= '9') || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || (c == ' ')
  }
  def directEncodeChar(c: Char): Char = if (c == ' ') '_' else c
  def directEncode(s: String): Option[String] = {
    if (s forall { directEncodeable(_) }) Some(s.map { directEncodeChar(_) } + "-") else None
  }
  def singleDirectEncodeable(c: Char) = ('0' <= c && c <= '9') || ('A' <= c && c <= 'Z')
  def hasSafeHead(s: String) = s.headOption match {
    case Some(c) => ('0' <= c && c <= '9') || ('A' <= c && c <= 'Z')
    case None => false
  }

  def directDecodeChar(c: Char): Char = if (c == '_') ' ' else c
  def directDecode(s: StringIter): String = s.until('-').map { directDecodeChar(_) }

  // This is the most gratuitious part of the project ever
  val indirectCharPairs = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_-nrtuvwy" zip "@!()=`>#|'^<\"&.%?:$+[]\\*,/_-\n\r\t{}~;"
  val decodeIndirectCharMap = Map(indirectCharPairs: _*)
  val encodeIndirectCharMap = Map((for ((s, c) <- indirectCharPairs) yield (c, s)): _*)
  def indirectEncodeChar(c: Char): String = {
    if (directEncodeable(c)) {
      directEncodeChar(c).toString
    } else encodeIndirectCharMap.get(c) match {
      case Some(c2) => s"-${c2}"
      case None => {
        val i = c.toInt
        s"-${i % 10}${Base64.encodeBase32(i / 10)}"
      }
    }
  }
  def indirectEncode(s: String): String = s.flatMap { indirectEncodeChar(_) } + "-z"
  def indirectDecode(s: StringIter): String = {
    val buf = new StringBuffer()
    var go = true
    while (go) {
      s.next() match {
        case None => throw new AssertionError("Premature ending in indirectDecode")
        case Some('-') => s.next() match {
          case None => throw new AssertionError("Premature ending in indirectDecode")
          case Some('z') => go = false
          case Some(c) => if (c.isDigit) {
            buf.append((Base64.decodeBase32(s) * 10 + c.toInt - '0').toChar)
          } else {
            buf.append(decodeIndirectCharMap(c))
          }
        }
        case Some(c) => buf.append(directDecodeChar(c))
      }
    }
    buf.toString
  }

  def encode(directEncodeSigil: Char, indirectEncodeSigil: Char, s: String): String = {
    if (s.length == 1 && singleDirectEncodeable(s.head)) s
    else directEncode(s) match {
      case Some(t) => directEncodeSigil.toString + t
      case None => indirectEncodeSigil.toString + indirectEncode(s)
    }
  }
}
