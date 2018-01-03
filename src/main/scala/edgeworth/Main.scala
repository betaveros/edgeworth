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
    def clearAll(): Unit = {
      pcm.cellMap = Map()
      masterSVG.clearAllGs()
    }
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

    val textInput = document.createElement("input").asInstanceOf[html.Input]
    textInput.setAttribute("type", "text")
    textInput.style.fontSize = "20pt"
    textInput.style.width = "1em"
    textInput.style.textAlign = "center"
    textInput.style.display = "none"
    textInput.style.opacity = "0.8"
    document.getElementById("svgwrap").appendChild(textInput)
    textInput.onkeydown = (event) => {
      if (event.key == "Enter") {
        event.preventDefault()
        putTextAtCursor(textInput.value)
        textInput.style.display = "none"
        masterSVG.focus()
      }
    }
    textInput.onblur = (event) => {
      putTextAtCursor(textInput.value)
      textInput.style.display = "none"
      masterSVG.focus()
    }

    masterSVG.onKeyPress((event) => {
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
        case "=" => cursor.selected match {
          case Some(p: CellPosition) => {
            event.preventDefault()
            textInput.style.display = "inline"
            textInput.style.position = "fixed"
            textInput.style.transform = "translate(-50%, -50%)"
            val (ux, uy) = grid.computePositionCenter(p)
            val (x, y) = masterSVG.screenSpaceCoords(ux, uy)
            textInput.style.left = x.toString ++ "px"
            textInput.style.top = y.toString ++ "px"
            textInput.value = ""
            textInput.focus()
          }
          case _ => ()
        }
      }
      // lift returns an Option[T] so that keys outside the partial function won't fail
      (moveReactions orElse basicDrawReactions orElse (digitMap andThen putTextAtCursor)).lift(event.key)
    })

    def updateBoundsDecoration(): Unit = {
      val w = 16 + gridBounds.colCount * 32
      val h = 16 + gridBounds.rowCount * 32
      masterSVG.setViewBox(-8, -8, w, h)
      val scale = 500.0 / (w max h)
      masterSVG.setScreenSize(w * scale, h * scale)
      decorator.decorate(masterSVG, grid, gridBounds)
    }
    def load(dec: GridDecorator, gb: GridBounds, pcm0: PositionContentMap) = {
      decorator = dec
      gridBounds = gb
      clearAll()
      for ((p, c) <- pcm0.cellMap) {
        putCellContent(p, c)
      }
      for ((p, c) <- pcm0.edgeMap) {
        putEdgeContent(p, c)
      }
      for ((p, c) <- pcm0.intersectionMap) {
        putIntersectionContent(p, c)
      }
      updateBoundsDecoration()
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

    def makeButton(text: String, handler: dom.Event => Unit) = {
      val button = document.createElement("button").asInstanceOf[html.Button]
      button.textContent = text
      button.onclick = handler
      button
    }

    val textarea = document.createElement("textarea").asInstanceOf[html.TextArea]
    val clearButton = makeButton("Clear", (event) => clearAll())
    val encodeButton = makeButton("Encode", (event) => {
      textarea.textContent = Codec.encode(decorator, gridBounds, pcm)
    })
    val setUrlEncodedButton = makeButton("Set URL to encoded", (event) => {
      document.location.hash = "#" ++ Codec.encode(decorator, gridBounds, pcm)
    })
    val decodeButton = makeButton("Decode", (event) => {
      val (dec, gb0, pcm0) = Codec.decode(textarea.value)
      load(dec, gb0, pcm0)
    })

    val wrapper = document.getElementById("wrap")

    val div = document.createElement("div")
    div.appendChild(clearButton)
    div.appendChild(encodeButton)
    div.appendChild(setUrlEncodedButton)
    div.appendChild(decodeButton)
    div.appendChild(textarea)
    wrapper.appendChild(div)

    val decdiv = document.createElement("div")
    for (d <- GridDecorator.allDecorators) {
      val rb = document.createElement("input").asInstanceOf[html.Input]
      val id = "decoration-" ++ d.id
      rb.setAttribute("type", "radio")
      rb.setAttribute("name", "decoration")
      rb.setAttribute("id", id)
      rb.onchange = (event) => {
        decorator = d
        updateBoundsDecoration()
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
