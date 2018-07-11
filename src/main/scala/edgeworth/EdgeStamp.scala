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
sealed abstract class InequalityEdgeStamp(mul: Int) extends EdgeStamp {
  def renderEdge(grid: SimpleGrid, epos: EdgePosition, color: Color) = {
    val EdgePosition(row, col, ori) = epos
    val (x, y) = grid.computePositionCenter(epos)
    val dx = mul * grid.fourth * ori.rowDelta
    val dy = mul * grid.fourth * ori.colDelta
    Seq(SVGUtil.vee(x - dx/2, y - dy/2, dx, dy, 3, color.toStyle))
  }
}
case object    LessEdgeStamp extends InequalityEdgeStamp(-1)
case object GreaterEdgeStamp extends InequalityEdgeStamp( 1)

object EdgeStamp {
  def encode(stamp: EdgeStamp): String = stamp match {
    case NormalEdgeStamp => "f"
    case TransverseEdgeStamp => "t"
    case CrossEdgeStamp => "x"
    case DotEdgeStamp => "d"
    case CircleEdgeStamp => "o"
    case LessEdgeStamp => "l"
    case GreaterEdgeStamp => "g"
  }
  def decode(s: StringIter): Option[EdgeStamp] = s.next() match {
    case Some('f') => Some(    NormalEdgeStamp)
    case Some('t') => Some(TransverseEdgeStamp)
    case Some('x') => Some(     CrossEdgeStamp)
    case Some('d') => Some(       DotEdgeStamp)
    case Some('o') => Some(    CircleEdgeStamp)
    case Some('l') => Some(      LessEdgeStamp)
    case Some('g') => Some(   GreaterEdgeStamp)
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
