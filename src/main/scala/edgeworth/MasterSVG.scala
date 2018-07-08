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

  val cellWrapper = newG()

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

  val edgeWrapper = newG()
  val intersectionWrapper = newG()

  var sublayerMap: Map[Int, (svg.G, svg.G, svg.G)] = Map()
  var gmap: Map[Int, Map[Position, svg.G]] = Map()

  def clearGs(z: Int): Unit = for (submap <- gmap.get(z); g <- submap.values) SVGUtil.clear(g)
  def clearAllGs(): Unit = for (submap <- gmap.values; g <- submap.values) SVGUtil.clear(g)

  def getSublayerG(z: Int, pos: Position) = {
    val (cg, eg, ig) = sublayerMap get z match {
      case Some(g3s) => g3s
      case None => {
        val cgg = SVGUtil.g()
        cellWrapper.appendChild(cgg)
        val egg = SVGUtil.g()
        edgeWrapper.appendChild(egg)
        val igg = SVGUtil.g()
        intersectionWrapper.appendChild(igg)
        val g3s = (cgg, egg, igg)
        sublayerMap += z -> g3s
        g3s
      }
    }
    pos match {
      case _: CellPosition => cg
      case _: EdgePosition => eg
      case _: IntersectionPosition => ig
    }
  }
  def getClearG(z: Int, pos: Position) = {
    val res = for (gzmap <- gmap get z; g <- gzmap get pos) yield g
    res match {
      case Some(g) => SVGUtil.clear(g)
      case None => {
        val g = SVGUtil.g()
        gmap += z -> (gmap.getOrElse(z, Map()) + (pos -> g))
        getSublayerG(z, pos).appendChild(g)
        g
      }
    }
  }

  // hacks (usually SVG elements can't have stuff attached or get focus, but
  // we put tabindex on it so it can; but Scala doesn't know that)
  def onKeyPress(handler: (dom.KeyboardEvent) => Unit) = {
    svgElement.asInstanceOf[html.Element].onkeypress = handler
  }
  def focus(): Unit = {
    svgElement.asInstanceOf[html.Element].focus()
  }
  def onClick(handler: (dom.MouseEvent) => Unit) = {
    svgElement.onclick = handler
  }


  def userSpaceCoords(x: Double, y: Double): (Double, Double) = {
    val pt = svgElement.createSVGPoint()
    pt.x = x
    pt.y = y
    val cursorpt = pt.matrixTransform(svgElement.getScreenCTM().inverse())
    (cursorpt.x, cursorpt.y)
  }
  def userSpaceCoords(event: dom.MouseEvent): (Double, Double) = {
    userSpaceCoords(event.clientX, event.clientY)
  }
  def screenSpaceCoords(x: Double, y: Double): (Double, Double) = {
    val pt = svgElement.createSVGPoint()
    pt.x = x
    pt.y = y
    val cursorpt = pt.matrixTransform(svgElement.getScreenCTM())
    (cursorpt.x, cursorpt.y)
  }
}
