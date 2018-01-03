package edgeworth

import org.scalajs.dom
import scala.collection.immutable.HashMap

// We'll stick to the traditional assignment. We're not parsing it to bytes,
// however.

object Base64 {
  val chars: String = ('A' to 'Z').mkString ++ ('a' to 'z').mkString ++ ('0' to '9').mkString ++ "-_"

  val charToIntMap: HashMap[Char, Int] = HashMap(chars.zipWithIndex: _*)

  // We could literally use both of the above as methods but that seems kind of
  // rude and haphazard to encapsulation
  def intToChar(i: Int) = chars(i)
  def charToInt(c: Char) = charToIntMap(c)

  def encodeBase32(i0: Int): String = {
    var s: String = intToChar(i0 & 0x1f).toString
    var i = i0 >>> 5
    while (i != 0) {
      s = intToChar((i & 0x1f) | 32) + s
      i >>>= 5
    }
    s
  }
  def decodeBase32(si: StringIter): Int = {
    var ret = 0
    while (true) si.next() match {
      case Some(ch) => {
        val c = charToInt(ch)
        ret = (ret << 5) | (c & 0x1f)
        if ((c & 0x20) == 0) return ret
      }
      case None => {
        Out.warn("Premature end while parsing base32")
        return ret
      }
    }
    throw new AssertionError("lolwat")
  }
}
