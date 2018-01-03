package edgeworth

import org.scalajs.dom.svg

sealed abstract class EdgeStamp {
  def renderEdge(grid: SimpleGrid, epos: EdgePosition, color: Color): Seq[svg.Element]
}

case object NormalEdgeStamp extends EdgeStamp {
  def renderEdge(grid: SimpleGrid, epos: EdgePosition, color: Color) = {
    val EdgePosition(row, col, ori) = epos
    Seq(SVGUtil.line(
      grid.computeX(col), grid.computeY(row),
      grid.computeX(col + ori.colDelta), grid.computeY(row + ori.rowDelta), 3, color.toStyle))
  }
}

case object TransverseEdgeStamp extends EdgeStamp {
  def renderEdge(grid: SimpleGrid, epos: EdgePosition, color: Color) = {
    val EdgePosition(row, col, ori) = epos
    Seq(SVGUtil.line(
      grid.computeCenterX(col - ori.rowDelta), grid.computeCenterY(row - ori.colDelta),
      grid.computeCenterX(col), grid.computeCenterY(row), 3, color.toStyle))
  }
}
case object CrossEdgeStamp extends EdgeStamp {
  def renderEdge(grid: SimpleGrid, epos: EdgePosition, color: Color) = {
    val (x, y) = grid.computePositionCenter(epos)
    val s = grid.eighth
    Seq(SVGUtil.x(x - s, y - s, x + s, y + s, 3, color.toStyle))
  }
}
case object DotEdgeStamp extends EdgeStamp {
  def renderEdge(grid: SimpleGrid, epos: EdgePosition, color: Color) = {
    val (x, y) = grid.computePositionCenter(epos)
    Seq(SVGUtil.circle(x, y, grid.eighth, color.toStyle))
  }
}
case object CircleEdgeStamp extends EdgeStamp {
  def renderEdge(grid: SimpleGrid, epos: EdgePosition, color: Color) = {
    val (x, y) = grid.computePositionCenter(epos)
    Seq(SVGUtil.circle(x, y, grid.eighth, "transparent", color.toStyle, 3))
  }
}
object EdgeStamp {
  def encode(stamp: EdgeStamp): String = stamp match {
    case NormalEdgeStamp => "f"
    case TransverseEdgeStamp => "t"
    case CrossEdgeStamp => "x"
    case DotEdgeStamp => "d"
    case CircleEdgeStamp => "o"
  }
  def decode(s: StringIter): Option[EdgeStamp] = s.next() match {
    case Some('f') => Some(    NormalEdgeStamp)
    case Some('t') => Some(TransverseEdgeStamp)
    case Some('x') => Some(     CrossEdgeStamp)
    case Some('d') => Some(       DotEdgeStamp)
    case Some('o') => Some(    CircleEdgeStamp)
    case Some('.') => None
    case Some(c) => {
      Out.warn("Unrecognized character while parsing edge stamp: " ++ c.toString)
      None
    }
    case None => {
      Out.warn("Premature end while parsing edge stamp")
      None
    }
  }
}
