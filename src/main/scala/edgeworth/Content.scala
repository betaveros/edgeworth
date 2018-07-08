package edgeworth

import org.scalajs.dom
import org.scalajs.dom.svg

case class CellContent(color: Color, stamp: CellStamp) {
  def render(grid: SimpleGrid, cpos: CellPosition): Seq[svg.Element] = {
    stamp.renderCell(grid, cpos, color)
  }
}
case class EdgeContent(color: Color, stamp: EdgeStamp) {
  def render(grid: SimpleGrid, epos: EdgePosition): Seq[svg.Element] = {
    stamp.renderEdge(grid, epos, color)
  }
}
case class IntersectionContent(color: Color, stamp: IntersectionStamp) {
  def render(grid: SimpleGrid, ipos: IntersectionPosition): Seq[svg.Element] = {
    stamp.renderIntersection(grid, ipos, color)
  }
}

object CellContent {
  def default = CellContent(Color.pencil, FillCellStamp)
  def text(s: String) = CellContent(Color.pencil, TextCellStamp(s))
  def encode(c: CellContent): String = Color.encode(c.color) ++ CellStamp.encode(c.stamp)
  def decode(s: StringIter): Option[CellContent] = {
    val color = Color.decode(s).getOrElse(Color.pencil)
    CellStamp.decode(s).map(CellContent(color, _))
  }
}
object EdgeContent {
  def default = EdgeContent(Color.pencil, NormalEdgeStamp)
  def encode(c: EdgeContent): String = Color.encode(c.color) ++ EdgeStamp.encode(c.stamp)
  def decode(s: StringIter): Option[EdgeContent] = {
    val color = Color.decode(s).getOrElse(Color.pencil)
    EdgeStamp.decode(s).map(EdgeContent(color, _))
  }
}
object IntersectionContent {
  def default = IntersectionContent(Color.pencil, SquareIntersectionStamp)
  def encode(c: IntersectionContent): String = Color.encode(c.color) ++ IntersectionStamp.encode(c.stamp)
  def decode(s: StringIter): Option[IntersectionContent] = {
    val color = Color.decode(s).getOrElse(Color.pencil)
    IntersectionStamp.decode(s).map(IntersectionContent(color, _))
  }
}
