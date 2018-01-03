package edgeworth

import org.scalajs.dom
import org.scalajs.dom.svg

object TextRenderer {
  def quote(s: String) = if (s.length > 0 && (s(0).isLower || s(0) == '\'')) "'" + s else s
  def render(s: String, pos: Position, grid: SimpleGrid): Seq[svg.Element] = pos match {
    case cp@CellPosition(row, col) => s match {
      case "" => Seq()
      case "f" => {
        Seq(SVGUtil.rect(grid.computeX(col), grid.computeY(row), grid.colWidth, grid.rowHeight, "#111"))
      }
      case "r" => {
        Seq(SVGUtil.rect(grid.computeX(col), grid.computeY(row), grid.colWidth, grid.rowHeight, "#c00"))
      }
      case "d" => {
        val (x, y) = grid.computePositionCenter(cp)
        Seq(SVGUtil.circle(x, y, 3, "#111"))
      }
      case "x" => {
        val x0 = grid.computeX(col)
        val y0 = grid.computeY(row)
        val w = grid.colWidth
        val h = grid.rowHeight
        val x1 = x0 + w * 0.25
        val y1 = y0 + h * 0.25
        val x2 = x0 + w * 0.75
        val y2 = y0 + h * 0.75
        Seq(SVGUtil.x(x0 + w*0.25, y0 + h*0.25, x0 + w*0.75, y0 + h*0.75, 3))
      }
      case _ => {
        Seq(SVGUtil.text(grid.computeCenterX(col), grid.computeCenterY(row), s, "20pt"))
      }
    }
    case ep@EdgePosition(row, col, ori) => s match {
      case "" => Seq()
      case "f" => {
        Seq(SVGUtil.line(
          grid.computeX(col), grid.computeY(row),
          grid.computeX(col + ori.colDelta), grid.computeY(row + ori.rowDelta), 3, "#111"))
      }
      case "t" => {
        Seq(SVGUtil.line(
          grid.computeCenterX(col - ori.rowDelta), grid.computeCenterY(row - ori.colDelta),
          grid.computeCenterX(col), grid.computeCenterY(row), 3, "#111"))
      }
      case "r" => {
        Seq(SVGUtil.line(
          grid.computeX(col), grid.computeY(row),
          grid.computeX(col + ori.colDelta), grid.computeY(row + ori.rowDelta), 3, "#c00"))
      }
      case "d" => {
        val (x, y) = grid.computePositionCenter(ep)
        Seq(SVGUtil.circle(x, y, 3, "#111"))
      }
      case "x" => {
        val (x, y) = grid.computePositionCenter(ep)
        Seq(SVGUtil.x(x - 4, y - 4, x + 4, y + 4, 3))
      }
      case _ => Seq()
    }
    case ip@IntersectionPosition(row, col) => s match {
      case "d" => {
        val (x, y) = grid.computePositionCenter(ip)
        Seq(SVGUtil.circle(x, y, 3, "#111"))
      }
      case "x" => {
        val (x, y) = grid.computePositionCenter(ip)
        Seq(SVGUtil.x(x - 4, y - 4, x + 4, y + 4, 3))
      }
      case _ => Seq()
    }
  }
}
