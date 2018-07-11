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
  def text(x: Double, y: Double, content: String, fontSize: String, fillStyle: String = "#111"): svg.Text = {
    val t = create("text").asInstanceOf[svg.Text]
    t.textContent = content
    t.style.fill = fillStyle
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
  def dPath(d: String, fill: String, strokeWidth: Double, stroke: String, strokeLinecap: String): svg.Path = {
    val path = SVGUtil.path()
    path.setAttribute("d", d)
    path.style.fill = fill
    path.style.stroke = stroke
    path.style.strokeWidth = strokeWidth.toString
    path.style.strokeLinecap = strokeLinecap
    path
  }
  def dSquarePath(d: String, strokeWidth: Double, stroke: String) = dPath(d, "transparent", strokeWidth, stroke, "square")
  def dRoundPath(d: String, strokeWidth: Double, stroke: String) = dPath(d, "transparent", strokeWidth, stroke, "round")
  def dFilledPath(d: String, fill: String) = dPath(d, fill, 0, "", "round")
  def x(x1: Double, y1: Double, x2: Double, y2: Double, strokeWidth: Double, stroke: String = "#111"): svg.Path = {
    dSquarePath(s"M${x1},${y1}L${x2},${y2}M${x1},${y2}L${x2},${y1}", strokeWidth, stroke)
  }
  def plus(x0: Double, y0: Double, dx: Double, dy: Double, strokeWidth: Double, stroke: String = "#111"): svg.Path = {
    dSquarePath(s"M${x0-dx},${y0}L${x0+dx},${y0}M${x0},${y0-dy}L${x0},${y0+dy}", strokeWidth, stroke)
  }
  def arrow(cx: Double, cy: Double, dx: Double, dy: Double, strokeWidth: Double, stroke: String = "#111"): Seq[svg.Path] = {
    val path1 = dSquarePath(s"M${cx-dx},${cy-dy}L${cx},${cy}M${cx-dy},${cy+dx}L${cx+dx},${cy+dy}L${cx+dy},${cy-dx}", strokeWidth, stroke)
    val path2 = dRoundPath(s"M${cx},${cy}L${cx+dx},${cy+dy}", strokeWidth, stroke)
    Seq(path1, path2)
  }
  def vee(x0: Double, y0: Double, dx: Double, dy: Double, strokeWidth: Double, stroke: String = "#111"): svg.Path = {
    dSquarePath(s"M${x0-dy},${y0+dx}L${x0+dx},${y0+dy}L${x0+dy},${y0-dx}", strokeWidth, stroke)
  }

  def star(cx: Double, cy: Double, scale: Double, fill: String): svg.Path = {
    val arms = 5
    val outerRadius = scale
    val innerRadius = outerRadius * (3 - Math.sqrt(5)) / 2
    val baseAngle = -Math.PI / 2
    val incAngle = Math.PI / arms
    def polarPair(incs: Int, r: Double): (Double, Double) = {
      val a = baseAngle + incs * incAngle
      (cx + Math.cos(a) * r, cy + Math.sin(a) * r)
    }

    val segments = for (i <- 0 until arms) yield {
      val (ox, oy) = polarPair(2*i, outerRadius)
      val (ix, iy) = polarPair(2*i + 1, innerRadius)
      s"${ox},${oy}L${ix},${iy}"
    }
    dFilledPath(s"M${segments.mkString("L")}Z", fill)
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
