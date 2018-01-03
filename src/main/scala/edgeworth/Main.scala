package edgeworth

import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.svg
import dom.document
import scala.collection.immutable.{ SortedMap, TreeMap }

object Main {
  def main(args: Array[String]): Unit = {
    val grid = SimpleGrid(32, 32, 0, 0)
    val masterSVG = new MasterSVG(document.getElementById("svge").asInstanceOf[svg.SVG])

    val cursor = new Cursor(Some(CellPosition(0, 0)))
    val cursorElt = SVGUtil.path()
    cursorElt.setAttribute("d", cursor.computePath(grid))
    cursorElt.style.fill = "transparent"
    cursorElt.style.stroke = "url(#cursor-gradient)"
    cursorElt.style.strokeWidth = "6"
    cursorElt.style.strokeLinejoin = "round"
    cursorElt.style.strokeOpacity = "0.75"
    masterSVG.append(cursorElt)

    def move(rd: Int, cd: Int) = {
      cursor.moveSelected(rd, cd)
      cursorElt.setAttribute("d", cursor.computePath(grid))
    }

    var decorator: GridDecorator = SolidDecorator
    var gridBounds: GridBounds = new GridBounds(10, 10)
    var pcm: PositionContentMap = new PositionContentMap()
    def putElements(pos: Position, elts: Seq[svg.Element]) = {
      val g = masterSVG.getClearG(pos)
      for (e <- elts) { g.appendChild(e) }
    }
    def clearCellContent(cpos: CellPosition) = putElements(cpos, Seq())
    def clearEdgeContent(epos: EdgePosition) = putElements(epos, Seq())
    def clearIntersectionContent(ipos: IntersectionPosition) = putElements(ipos, Seq())
    def putCellContent(cpos: CellPosition, cc: CellContent) = {
      pcm.putCellContent(cpos, cc)
      putElements(cpos, cc.render(grid, cpos))
    }
    def putEdgeContent(epos: EdgePosition, ec: EdgeContent) = {
      pcm.putEdgeContent(epos, ec)
      putElements(epos, ec.render(grid, epos))
    }
    def putIntersectionContent(ipos: IntersectionPosition, ic: IntersectionContent) = {
      pcm.putIntersectionContent(ipos, ic)
      putElements(ipos, ic.render(grid, ipos))
    }

    def fillEdge(epos: EdgePosition, color: Color = Color.pencil) = putEdgeContent(epos, EdgeContent(color, NormalEdgeStamp))
    def transverseEdge(epos: EdgePosition) = putEdgeContent(epos, EdgeContent(Color.pencil, TransverseEdgeStamp))

    def moveAndDraw(rd: Int, cd: Int) = {
      cursor.selected foreach (se => {
        val dpos = se.deltaPosition(rd, cd)
        (se, dpos) match {
          case (_: CellPosition, epos: EdgePosition) => {
            transverseEdge(epos)
            cursor.selected = Some(se.deltaPosition(2*rd, 2*cd))
          }
          case (_: IntersectionPosition, epos: EdgePosition) => {
            fillEdge(epos)
            cursor.selected = Some(se.deltaPosition(2*rd, 2*cd))
          }
          case (epos: EdgePosition, _: CellPosition) => {
            transverseEdge(epos)
            cursor.selected = Some(dpos)
          }
          case (epos: EdgePosition, _: IntersectionPosition) => {
            fillEdge(epos)
            cursor.selected = Some(dpos)
          }
          case _ => throw new AssertionError("moveAndDrawSelected failed!")
        }
      })
      cursorElt.setAttribute("d", cursor.computePath(grid))
    }
    def putTextAtCursor(s: String) = cursor.selected match {
      case Some(p@CellPosition(row, col)) => putCellContent(p, CellContent(Color.pencil, TextCellStamp(s)))
      case _ => ()
    }
    val digitMap: Map[String, String] = Map((for (i <- 0 to 9) yield i.toString -> i.toString): _*)

    document.onkeypress = (event) => {
      val cPrefix = if (event.ctrlKey) "C-" else ""
      val sPrefix = if (event.shiftKey) "S-" else ""
      val procKey = event.key match {
        case "ArrowUp"    => cPrefix + sPrefix + "Up"
        case "ArrowDown"  => cPrefix + sPrefix + "Down"
        case "ArrowLeft"  => cPrefix + sPrefix + "Left"
        case "ArrowRight" => cPrefix + sPrefix + "Right"
        case _ => event.key
      }
      val moveReactions: PartialFunction[String, Unit] = (s) => s match {
        case "k" | "Up"    => move(-1, 0)
        case "j" | "Down"  => move(1, 0)
        case "h" | "Left"  => move(0, -1)
        case "l" | "Right" => move(0, 1)
        case "K" | "S-Up"    => moveAndDraw(-1, 0)
        case "J" | "S-Down"  => moveAndDraw(1, 0)
        case "H" | "S-Left"  => moveAndDraw(0, -1)
        case "L" | "S-Right" => moveAndDraw(0, 1)
      }
      val basicDrawReactions: PartialFunction[String, Unit] = (s) => s match {
        case " " => cursor.selected match {
          case Some(p: CellPosition) => clearCellContent(p)
          case Some(p: EdgePosition) => clearEdgeContent(p)
          case Some(p: IntersectionPosition) => clearIntersectionContent(p)
          case None => ()
        }
        case "f" => cursor.selected match {
          case Some(p: CellPosition) => putCellContent(p, CellContent(Color.pencil, FillCellStamp))
          case Some(p: EdgePosition) => fillEdge(p)
          case Some(p: IntersectionPosition) => putIntersectionContent(p, IntersectionContent(Color.pencil, SquareIntersectionStamp))
          case _ => ()
        }
        case "r" => cursor.selected match {
          case Some(p: CellPosition) => putCellContent(p, CellContent(Color.red, FillCellStamp))
          case Some(p: EdgePosition) => fillEdge(p, Color.red)
          case Some(p: IntersectionPosition) => putIntersectionContent(p, IntersectionContent(Color.red, SquareIntersectionStamp))
          case _ => ()
        }
        case "d" | "." => cursor.selected match {
          case Some(p: CellPosition) => putCellContent(p, CellContent(Color.pencil, DotCellStamp))
          case Some(p: EdgePosition) => putEdgeContent(p, EdgeContent(Color.pencil, DotEdgeStamp))
          case Some(p: IntersectionPosition) => putIntersectionContent(p, IntersectionContent(Color.pencil, DotIntersectionStamp))
          case _ => ()
        }
        case "o" => cursor.selected match {
          case Some(p: CellPosition) => putCellContent(p, CellContent(Color.pencil, CircleCellStamp))
          case Some(p: EdgePosition) => putEdgeContent(p, EdgeContent(Color.pencil, CircleEdgeStamp))
          case Some(p: IntersectionPosition) => putIntersectionContent(p, IntersectionContent(Color.pencil, CircleIntersectionStamp))
          case _ => ()
        }
        case "x" => cursor.selected match {
          case Some(p: CellPosition) => putCellContent(p, CellContent(Color.pencil, CrossCellStamp))
          case Some(p: EdgePosition) => putEdgeContent(p, EdgeContent(Color.pencil, CrossEdgeStamp))
          case Some(p: IntersectionPosition) => putIntersectionContent(p, IntersectionContent(Color.pencil, CrossIntersectionStamp))
          case _ => ()
        }
      }
      // lift returns an Option[T] so that keys outside the partial function won't fail
      (moveReactions orElse basicDrawReactions orElse (digitMap andThen putTextAtCursor)).lift(event.key)
    }

    def updateDecoration(): Unit = {
      decorator.decorate(masterSVG, grid, gridBounds)
    }
    def load(dec: GridDecorator, gb: GridBounds, pcm0: PositionContentMap) = {
      decorator = dec
      gridBounds = gb
      for ((p, c) <- pcm0.cellMap) {
        putCellContent(p, c)
      }
      for ((p, c) <- pcm0.edgeMap) {
        putEdgeContent(p, c)
      }
      for ((p, c) <- pcm0.intersectionMap) {
        putIntersectionContent(p, c)
      }
      updateDecoration()
    }
    val hashStr = document.location.hash
    def loadHashStr(hashStr: String) = {
      if (hashStr.length > 0) {
        // cut off the '#'
        val (dec, gb0, pcm0) = Codec.decode(hashStr.tail)
        load(dec, gb0, pcm0)
      } else {
        load(SolidDecorator, GridBounds(10, 10), new PositionContentMap())
      }
    }
    loadHashStr(document.location.hash)
    dom.window.onhashchange = (event) => {
      loadHashStr(document.location.hash)
    }

    val encodeButton = document.createElement("button").asInstanceOf[html.Button]
    encodeButton.textContent = "encode"
    val setUrlEncodedButton = document.createElement("button").asInstanceOf[html.Button]
    setUrlEncodedButton.textContent = "set URL to encoded"
    val decodeButton = document.createElement("button").asInstanceOf[html.Button]
    decodeButton.textContent = "decode"
    val textarea = document.createElement("textarea").asInstanceOf[html.TextArea]

    val wrapper = document.getElementById("wrap")

    val div = document.createElement("div")
    div.appendChild(encodeButton)
    div.appendChild(setUrlEncodedButton)
    div.appendChild(decodeButton)
    div.appendChild(textarea)
    wrapper.appendChild(div)

    encodeButton.onclick = (event) => {
      textarea.textContent = Codec.encode(decorator, gridBounds, pcm)
    }
    setUrlEncodedButton.onclick = (event) => {
      document.location.hash = "#" ++ Codec.encode(decorator, gridBounds, pcm)
    }
    decodeButton.onclick = (event) => {
      val (dec, gb0, pcm0) = Codec.decode(textarea.value)
      load(dec, gb0, pcm0)
    }

    val decdiv = document.createElement("div")
    for (d <- GridDecorator.allDecorators) {
      val rb = document.createElement("input").asInstanceOf[html.Input]
      val id = "decoration-" ++ d.id
      rb.setAttribute("type", "radio")
      rb.setAttribute("name", "decoration")
      rb.setAttribute("id", id)
      rb.onchange = (event) => {
        decorator = d
        updateDecoration()
      }
      decdiv.appendChild(rb)
      val lb = document.createElement("label")
      lb.setAttribute("for", id)
      lb.textContent = d.description
      decdiv.appendChild(lb)
    }
    wrapper.appendChild(decdiv)

    masterSVG.onClick((event) => {
      val (x, y) = masterSVG.userSpaceCoords(event)
      cursor.selected = Some(grid.computePosition(x, y))
      cursorElt.setAttribute("d", cursor.computePath(grid))
    })
  }
}
