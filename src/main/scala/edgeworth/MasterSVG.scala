package edgeworth

import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.svg
import dom.document

class MasterSVG(val svgElement: svg.SVG) {

  def append(e: svg.Element): Unit = {
    svgElement.appendChild(e)
  }
  def setScreenSize(w: Double, h: Double) = {
    svgElement.setAttribute("width", w.toString)
    svgElement.setAttribute("height", h.toString)
  }
  def setViewBox(x: Double, y: Double, w: Double, h: Double) = {
    svgElement.setAttribute("viewBox", Seq(x, y, w, h).mkString(" "))
  }
  def newG(): svg.G = {
    val g = SVGUtil.g()
    append(g)
    g
  }

  // svgElement.setAttribute("width", "315")
  // svgElement.setAttribute("height", "315")
  // svgElt.width.baseVal.value = 300.0
  // svgElt.height.baseVal.value = 500.0

  val cellContentG = newG()

  val decorationG = newG()

  def clearDecoration(): Unit = SVGUtil.clear(decorationG)
  def decorateBorder(grid: SimpleGrid, gridBounds: GridBounds) = {
    val x0 = grid.computeX(0)
    val y0 = grid.computeY(0)
    val x1 = grid.computeX(gridBounds.colCount)
    val y1 = grid.computeY(gridBounds.rowCount)
    val borderRect = SVGUtil.rect(x0, y0, x1 - x0, y1 - y0, "transparent")
    borderRect.style.stroke = "black"
    borderRect.style.strokeWidth = "3"
    decorationG.appendChild(borderRect)
  }
  def decorateLines(grid: SimpleGrid, gridBounds: GridBounds, color: String, strokeWidth: Double, dasharray: String = "", opacity: Double = 1.0, includeBorder: Boolean = false) = {
    val x0 = grid.computeX(0)
    val y0 = grid.computeY(0)
    val x1 = grid.computeX(gridBounds.colCount)
    val y1 = grid.computeY(gridBounds.rowCount)
    val borderOffset = if (includeBorder) 0 else 1
    for (r <- borderOffset to (gridBounds.rowCount - borderOffset)) {
      val y = grid.computeY(r)
      decorationG.appendChild(SVGUtil.line(x0, y, x1, y, strokeWidth, color, dasharray, opacity.toString))
    }
    for (c <- borderOffset to (gridBounds.colCount - borderOffset)) {
      val x = grid.computeX(c)
      decorationG.appendChild(SVGUtil.line(x, y0, x, y1, strokeWidth, color, dasharray, opacity.toString))
    }
  }
  def decorateIntersections(grid: SimpleGrid, gridBounds: GridBounds) = {
    for (r <- 0 to gridBounds.rowCount) {
      val y = grid.computeY(r)
      for (c <- 0 to gridBounds.colCount) {
        val x = grid.computeX(c)
        decorationG.appendChild(SVGUtil.circle(x, y, grid.eighth, "black"))
      }
    }
  }

  val edgeContentG = newG()
  val intersectionContentG = newG()

  var gmap: Map[Position, svg.G] = Map[Position, svg.G]()

  def clearAllGs(): Unit = for (g <- gmap.values) SVGUtil.clear(g)
  def getClearG(pos: Position) = gmap get pos match {
    case Some(g) => SVGUtil.clear(g)
    case None => {
      val g = SVGUtil.g()
      gmap += pos -> g
      (pos match {
        case _: CellPosition => cellContentG
        case _: EdgePosition => edgeContentG
        case _: IntersectionPosition => intersectionContentG
      }).appendChild(g)
      g
    }
  }

  def onKeyPress(handler: (dom.KeyboardEvent) => Unit) = {
    // hack (usually SVG elements can't have stuff attached but we put
    // tabindex on it so it can; but Scala doesn't know that)
    svgElement.asInstanceOf[html.Element].onkeypress = handler
  }
  def onClick(handler: (dom.MouseEvent) => Unit) = {
    svgElement.onclick = handler
  }

  def userSpaceCoords(event: dom.MouseEvent): (Double, Double) = {
    val pt = svgElement.createSVGPoint()
    pt.x = event.clientX
    pt.y = event.clientY
    val cursorpt = pt.matrixTransform(svgElement.getScreenCTM().inverse())
    (cursorpt.x, cursorpt.y)
  }
}
