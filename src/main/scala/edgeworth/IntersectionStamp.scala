package edgeworth

import org.scalajs.dom.svg

sealed abstract class IntersectionStamp {
  def renderIntersection(grid: SimpleGrid, ipos: IntersectionPosition, color: Color): Seq[svg.Element]
}

case object CrossIntersectionStamp extends IntersectionStamp {
  def renderIntersection(grid: SimpleGrid, ipos: IntersectionPosition, color: Color) = {
    val (x, y) = grid.computePositionCenter(ipos)
    val s = grid.eighth
    Seq(SVGUtil.x(x - s, y - s, x + s, y + s, 3, color.toStyle))
  }
}
case object DotIntersectionStamp extends IntersectionStamp {
  def renderIntersection(grid: SimpleGrid, ipos: IntersectionPosition, color: Color) = {
    val (x, y) = grid.computePositionCenter(ipos)
    Seq(SVGUtil.circle(x, y, grid.eighth, color.toStyle))
  }
}
case object SquareIntersectionStamp extends IntersectionStamp {
  def renderIntersection(grid: SimpleGrid, ipos: IntersectionPosition, color: Color) = {
    val (x, y) = grid.computePositionCenter(ipos)
    val s = grid.eighth
    Seq(SVGUtil.rect(x - s, y - s, 2 * s, 2 * s, color.toStyle))
  }
}
case object CircleIntersectionStamp extends IntersectionStamp {
  def renderIntersection(grid: SimpleGrid, ipos: IntersectionPosition, color: Color) = {
    val (x, y) = grid.computePositionCenter(ipos)
    Seq(SVGUtil.circle(x, y, grid.eighth, "transparent", color.toStyle, 3))
  }
}
object IntersectionStamp {
  def encode(stamp: IntersectionStamp): String = stamp match {
    case SquareIntersectionStamp => "f"
    case CrossIntersectionStamp => "x"
    case DotIntersectionStamp => "d"
    case CircleIntersectionStamp => "o"
  }
  def decode(s: StringIter): Option[IntersectionStamp] = s.next() match {
    case Some('f') => Some(SquareIntersectionStamp)
    case Some('x') => Some( CrossIntersectionStamp)
    case Some('d') => Some(   DotIntersectionStamp)
    case Some('o') => Some(CircleIntersectionStamp)
    case Some('.') => None
    case Some(c) => {
      Out.warn("Unrecognized character while parsing intersection stamp: " ++ c.toString)
      None
    }
    case None => {
      Out.warn("Premature end while parsing intersection stamp")
      None
    }
  }
}
