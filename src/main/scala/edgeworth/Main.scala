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
    val cursorElt = SVGUtil.cursor()
    cursorElt.setAttribute("d", cursor.computePath(grid))
    masterSVG.append(cursorElt)

    val statusSpan = document.getElementById("status-span")
    statusSpan.textContent = "Hello!"

    var lockFunction: Position => Position = identity[Position]
    var lockMultiplier: Int = 1

    var decorator: GridDecorator = SolidDecorator
    var gridBounds: GridBounds = new GridBounds(10, 10)
    var pcmSeq: Seq[PositionContentMap] = Seq(new PositionContentMap())
    var currentPCMIndex = 0

    def currentZ = currentPCMIndex
    def currentPCM = pcmSeq(currentPCMIndex)
    def updateStatus() {
      statusSpan.textContent = s"${cursor.shortStatus} Layer ${currentPCMIndex + 1}/${pcmSeq.length}"
    }
    def ensureLockAndUpdate() {
      cursor.selected = cursor.selected map lockFunction
      cursorElt.setAttribute("d", cursor.computePath(grid))
      updateStatus()
    }
    def setLockFunctionMultiplier(f: Position => Position, m: Int) = {
      lockFunction = f
      lockMultiplier = m
      ensureLockAndUpdate()
    }

    def move(rd: Int, cd: Int) = {
      cursor.moveSelected(rd, cd)
      ensureLockAndUpdate()
    }
    def moveWithMultiplier(rd: Int, cd: Int) = {
      move(rd * lockMultiplier, cd * lockMultiplier)
    }

    def putElements(z: Int, pos: Position, elts: Seq[svg.Element]) = {
      val g = masterSVG.getClearG(z, pos)
      for (e <- elts) { g.appendChild(e) }
    }
    def clearCellContent(cpos: CellPosition) = putElements(currentZ, cpos, Seq())
    def clearEdgeContent(epos: EdgePosition) = putElements(currentZ, epos, Seq())
    def clearIntersectionContent(ipos: IntersectionPosition) = putElements(currentZ, ipos, Seq())
    def clearCurrent(): Unit = {
      currentPCM.clear()
      masterSVG.clearGs(currentZ)
      updateStatus()
    }
    def clearAll(): Unit = {
      pcmSeq foreach { _.clear() }
      masterSVG.clearAllGs()
      updateStatus()
    }
    def putCellContent(cpos: CellPosition, cc: CellContent) = {
      currentPCM.putCellContent(cpos, cc)
      putElements(currentZ, cpos, cc.render(grid, cpos))
    }
    def putEdgeContent(epos: EdgePosition, ec: EdgeContent) = {
      currentPCM.putEdgeContent(epos, ec)
      putElements(currentZ, epos, ec.render(grid, epos))
    }
    def putIntersectionContent(ipos: IntersectionPosition, ic: IntersectionContent) = {
      currentPCM.putIntersectionContent(ipos, ic)
      putElements(currentZ, ipos, ic.render(grid, ipos))
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
      ensureLockAndUpdate()
    }
    def textToCellStamp(s: String) = s match {
      case "-^" =>    UpArrowCellStamp
      case "-v" =>  DownArrowCellStamp
      case "<-" =>  LeftArrowCellStamp
      case "->" => RightArrowCellStamp

      case _    => TextCellStamp(s)
    }
    def putTextAtCursor(s: String) = cursor.selected match {
      case Some(p@CellPosition(row, col)) => putCellContent(p, CellContent(Color.pencil, textToCellStamp(s)))
      case _ => ()
    }
    val digitMap: Map[String, String] = Map((for (i <- 0 to 9) yield i.toString -> i.toString): _*)

    val textInput = document.createElement("input").asInstanceOf[html.Input]
    textInput.setAttribute("type", "text")
    textInput.setAttribute("class", "cell")
    textInput.style.fontSize = "22pt"
    textInput.style.display = "none"
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

    val command = document.getElementById("command").asInstanceOf[html.Input]

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
        case "k" | "Up"    => moveWithMultiplier(-1, 0)
        case "j" | "Down"  => moveWithMultiplier(1, 0)
        case "h" | "Left"  => moveWithMultiplier(0, -1)
        case "l" | "Right" => moveWithMultiplier(0, 1)
        case "K" | "S-Up"    => moveAndDraw(-1, 0)
        case "J" | "S-Down"  => moveAndDraw(1, 0)
        case "H" | "S-Left"  => moveAndDraw(0, -1)
        case "L" | "S-Right" => moveAndDraw(0, 1)
      }
      val metaReactions: PartialFunction[String, Unit] = (s) => s match {
        case ":" => command.focus()
        case "[" => {
          currentPCMIndex -= 1
          if (currentPCMIndex < 0) currentPCMIndex = pcmSeq.length - 1
          updateStatus()
        }
        case "]" => {
          currentPCMIndex += 1
          if (currentPCMIndex >= pcmSeq.length) currentPCMIndex = 0
          updateStatus()
        }
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
        case "-" => cursor.selected match {
          case Some(p: CellPosition) => putCellContent(p, CellContent(Color.pencil, HorizontalLineCellStamp))
          case _ => ()
        }
        case "|" => cursor.selected match {
          case Some(p: CellPosition) => putCellContent(p, CellContent(Color.pencil, VerticalLineCellStamp))
          case _ => ()
        }
        case "/" => cursor.selected match {
          case Some(p: CellPosition) => putCellContent(p, CellContent(Color.pencil, ForwardSlashLineCellStamp))
          case _ => ()
        }
        case "\\" => cursor.selected match {
          case Some(p: CellPosition) => putCellContent(p, CellContent(Color.pencil, BackwardSlashLineCellStamp))
          case _ => ()
        }
        case "=" => cursor.selected match {
          case Some(p: CellPosition) => {
            textInput.style.display = "inline"
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
      Out.log(event.key)
      // lift returns an Option[T] so that keys outside the partial function won't fail
      (moveReactions orElse basicDrawReactions orElse metaReactions orElse (digitMap andThen putTextAtCursor)).lift(event.key) match {
        case Some(()) => event.preventDefault()
        case None => ()
      }
    })

    def updateBoundsDecoration(): Unit = {
      val w = 16 + gridBounds.colCount * 32
      val h = 16 + gridBounds.rowCount * 32
      masterSVG.setViewBox(-8, -8, w, h)
      val scale = 500.0 / (w max h)
      masterSVG.setScreenSize(w * scale, h * scale)
      decorator.decorate(masterSVG, grid, gridBounds)
    }
    def load(state: CodecState) = {
      decorator = state.gridDecorator
      gridBounds = state.gridBounds
      clearAll()

      pcmSeq = Seq()
      for ((pcm0, i) <- state.pcms.zipWithIndex) {
        pcmSeq :+= new PositionContentMap()
        for ((p, c) <- pcm0.cellMap) {
          putCellContent(p, c)
        }
        for ((p, c) <- pcm0.edgeMap) {
          putEdgeContent(p, c)
        }
        for ((p, c) <- pcm0.intersectionMap) {
          putIntersectionContent(p, c)
        }
      }
      updateBoundsDecoration()
      updateStatus()
    }
    val hashStr = document.location.hash
    def loadHashStr(hashStr: String) = {
      if (hashStr.length > 0) {
        // cut off the '#'
        load(Codec.decode(hashStr.tail))
      } else {
        load(Codec.EMPTY_STATE)
      }
    }
    loadHashStr(document.location.hash)
    dom.window.onhashchange = (event) => {
      loadHashStr(document.location.hash)
    }

    def makeButton(text: String, handler: dom.Event => Unit) = {
      val button = document.createElement("button").asInstanceOf[html.Button]
      button.setAttribute("class", "btn")
      button.textContent = text
      button.onclick = handler
      button
    }

    val textarea = document.createElement("textarea").asInstanceOf[html.TextArea]
    val clearButton = makeButton("Clear", (event) => clearAll())
    val encodeButton = makeButton("Encode", (event) => {
      textarea.textContent = Codec.encode(CodecState(decorator, gridBounds, pcmSeq))
    })
    val setUrlEncodedButton = makeButton("Set URL to encoded", (event) => {
      document.location.hash = "#" ++ Codec.encode(CodecState(decorator, gridBounds, pcmSeq))
    })
    val decodeButton = makeButton("Decode", (event) => {
      load(Codec.decode(textarea.value))
    })

    val modal = document.getElementById("modal-body")

    val div = document.createElement("div")
    div.appendChild(clearButton)
    div.appendChild(encodeButton)
    div.appendChild(setUrlEncodedButton)
    div.appendChild(decodeButton)
    div.appendChild(textarea)
    modal.appendChild(div)

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
    modal.appendChild(decdiv)

    val lockdiv = document.createElement("div")
    lockdiv.appendChild(document.createTextNode("Lock cursor to:"))
    val locks: Seq[(String, Position => Position, Int)] = Seq(
      ("none", identity[Position], 1),
      ("cells", _.roundToCell, 2),
      ("intersections", _.roundToIntersection, 2))
    for ((s, f, m) <- locks) {
      val rb = document.createElement("input").asInstanceOf[html.Input]
      val id = "lock-" ++ s
      rb.setAttribute("type", "radio")
      rb.setAttribute("name", "decoration")
      rb.setAttribute("id", id)
      rb.onchange = (event) => setLockFunctionMultiplier(f, m)
      lockdiv.appendChild(rb)
      val lb = document.createElement("label")
      lb.setAttribute("for", id)
      lb.textContent = s
      lockdiv.appendChild(lb)
    }
    modal.appendChild(lockdiv)

    def addPCM(): Unit = {
      pcmSeq :+= new PositionContentMap()
      currentPCMIndex = pcmSeq.length - 1
      updateStatus()
    }

    val nurikabeButton = makeButton("To Nurikabe", (event) => {
      decorator = SolidDecorator
      updateBoundsDecoration()
      addPCM()
    })
    modal.appendChild(nurikabeButton)
    val slitherlinkButton = makeButton("To Slitherlink", (event) => {
      decorator = DotDecorator
      updateBoundsDecoration()
      addPCM()
    })
    modal.appendChild(slitherlinkButton)
    println("appended")

    val modalBg = document.getElementById("modal-bg").asInstanceOf[html.Element]
    document.getElementById("status-right").appendChild(makeButton("\u2191", (event) => {
      modalBg.style.display = "block"
    }))
    modal.appendChild(makeButton("Close", (event) => {
      modalBg.style.display = "none"
    }))

    command.onkeydown = (event) => {
      if (event.key == "Enter") {
        event.preventDefault()
        command.value match {
          case "clear" => clearCurrent()
          case "clearall" => clearAll()
          case "newlayer" => addPCM()
          case e => Out.error("Unrecognized command: " ++ e)
          // TODO: present error in user-friendly way
        }
        command.value = ""
        masterSVG.focus()
      }
    }
    command.onblur = (event) => {
      command.value = ""
      masterSVG.focus()
    }

    masterSVG.onClick((event) => {
      val (x, y) = masterSVG.userSpaceCoords(event)
      cursor.selected = Some(grid.computePosition(x, y))
      ensureLockAndUpdate()
    })
    updateStatus()
  }
}
