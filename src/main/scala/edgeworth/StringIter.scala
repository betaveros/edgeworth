package edgeworth

class StringIter(string: String) {
  var index = 0
  def hasNext(): Boolean = index < string.length
  def peek(): Option[Char] = string.lift(index)
  def next(): Option[Char] = {
    val ret = peek
    index += 1
    ret
  }
  def back(): Unit = {
    assert(index > 0)
    index -= 1
  }
  def nextN(count: Int): Option[String] = {
    if (index + count <= string.length) {
      val ret = string.substring(index, index + count)
      index += count
      Some(ret)
    } else None
  }
  def nextDigits(): String = {
    val buf = new StringBuffer()
    var go = true
    while (go && index < string.length) {
      val c = string(index)
      if (c.isDigit) {
        buf.append(c)
        index += 1
      } else {
        go = false
      }
    }
    buf.toString
  }
  def nextDigitsInt(): Int = {
    try {
      nextDigits().toInt
    } catch {
      case e: NumberFormatException => {
        Out.warn("Failed trying to parse digits: " ++ e.toString)
        0
      }
    }
  }
  def until(sentinel: Char): String = {
    val buf = new StringBuffer()
    var go = true
    while (go) {
      if (index >= string.length) {
        Out.warn("Premature ending while reading until " ++ sentinel.toString)
        return buf.toString
      }
      val c = string(index)
      if (c == sentinel) {
        go = false
      } else {
        buf.append(c)
      }
      index += 1
    }
    buf.toString
  }
}
