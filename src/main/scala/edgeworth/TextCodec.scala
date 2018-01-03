package edgeworth

object TextCodec {
  // This is not the most type-safe way to do this
  def directEncodeable(c: Char): Boolean = {
    ('0' <= c && c <= '9') || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || (c == ' ')
  }
  def directEncodeChar(c: Char): Char = if (c == ' ') '_' else c
  def directEncode(s: String): Option[String] = {
    if (s forall { directEncodeable(_) }) Some(s.map { directEncodeChar(_) }) else None
  }
  def upperOrDigit(c: Char) = ('0' <= c && c <= '9') || ('A' <= c && c <= 'Z')
  def hasSafeHead(s: String) = s.headOption match {
    case Some(c) => ('0' <= c && c <= '9') || ('A' <= c && c <= 'Z')
    case None => false
  }

  def directDecodeChar(c: Char): Char = if (c == '_') ' ' else c
  def directDecode(s: StringIter): String = s.until('-').map { directDecodeChar(_) }
}
