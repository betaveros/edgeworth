package edgeworth

import org.scalajs.dom
import org.scalajs.dom.svg
import dom.document

object SVGUtil {
  val ns = "http://www.w3.org/2000/svg"

  def create(tagName: String): svg.Element = document.createElementNS(ns, tagName).asInstanceOf[svg.Element]
  def rect(x: Double, y: Double, w: Double, h: Double, fill: String): svg.RectElement = {
    val r = create("rect").asInstanceOf[svg.RectElement]
    r.x.baseVal.value = x
    r.y.baseVal.value = y
    r.width.baseVal.value = w
    r.height.baseVal.value = h
    r.style.fill = fill
    r
  }
  def circle(cx: Double, cy: Double, r: Double, fill: String, stroke: String = "", strokeWidth: Double = 0): svg.Circle = {
    val c = create("circle").asInstanceOf[svg.Circle]
    c.cx.baseVal.value = cx
    c.cy.baseVal.value = cy
    c.r.baseVal.value = r
    c.style.fill = fill
    c.style.stroke = stroke
    c.style.strokeWidth = strokeWidth.toString
    c
  }
  def g(): svg.G = create("g").asInstanceOf[svg.G]
  def clear(g: svg.G): svg.G = {
    while (g.lastChild != null) { g.removeChild(g.lastChild) }
    g
  }
  def line(x1: Double, y1: Double, x2: Double, y2: Double,
    strokeWidth: Double = 1, stroke: String = "#111", dasharray: String = "", opacity: String = ""): svg.Line = {

    val r = create("line").asInstanceOf[svg.Line]
    r.x1.baseVal.value = x1
    r.y1.baseVal.value = y1
    r.x2.baseVal.value = x2
    r.y2.baseVal.value = y2
    r.style.strokeWidth = strokeWidth.toString
    r.style.stroke = stroke
    r.style.strokeDasharray = dasharray
    r.style.strokeLinecap = "square"
    r.style.strokeLinejoin = "square"
    r.style.opacity = opacity
    r
  }
  def text(x: Double, y: Double, content: String, fontSize: String): svg.Text = {
    val t = create("text").asInstanceOf[svg.Text]
    t.textContent = content
    t.style.fontSize = fontSize
    t.style.fontFamily = "Arial"
    t.style.textAnchor = "middle"
    t.style.dominantBaseline = "central"
    t.setAttribute("x", x.toString)
    t.setAttribute("y", y.toString)
    t
  }
  def path(): svg.Path = {
    create("path").asInstanceOf[svg.Path]
  }
  def x(x1: Double, y1: Double, x2: Double, y2: Double, strokeWidth: Double, stroke: String = "#111"): svg.Path = {
    val path = SVGUtil.path()
    path.setAttribute("d", s"M${x1},${y1}L${x2},${y2}M${x1},${y2}L${x2},${y1}")
    path.style.stroke = stroke
    path.style.strokeWidth = strokeWidth.toString
    path.style.strokeLinecap = "square"
    path
  }
  def arrow(cx: Double, cy: Double, dx: Double, dy: Double, strokeWidth: Double, stroke: String = "#111"): Seq[svg.Path] = {
    val path1 = SVGUtil.path()
    path1.setAttribute("d", s"M${cx-dx},${cy-dy}L${cx},${cy}M${cx-dy},${cy+dx}L${cx+dx},${cy+dy}L${cx+dy},${cy-dx}")
    path1.style.stroke = stroke
    path1.style.fill = "transparent"
    path1.style.strokeWidth = strokeWidth.toString
    path1.style.strokeLinecap = "square"

    val path2 = SVGUtil.path()
    path2.setAttribute("d", s"M${cx},${cy}L${cx+dx},${cy+dy}")
    path2.style.stroke = stroke
    path1.style.fill = "transparent"
    path2.style.strokeWidth = strokeWidth.toString
    path2.style.strokeLinecap = "round"

    Seq(path1, path2)
  }

  def cursor(): svg.Path = {
    val ret = SVGUtil.path()
    ret.style.fill = "transparent"
    ret.style.stroke = "url(#cursor-gradient)"
    ret.style.strokeWidth = "6"
    ret.style.strokeLinejoin = "round"
    ret.style.strokeOpacity = "0.75"
    ret
  }
}
