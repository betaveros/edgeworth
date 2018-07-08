package edgeworth

import org.scalajs.dom
import org.scalajs.dom.html
import dom.document

object HTMLEncoder {
  def apply(state: CodecState): html.Table = {
    val rowCount = state.gridBounds.rowCount
    val colCount = state.gridBounds.colCount
    val tds = for (r <- 1 to rowCount) yield {
      for (c <- 1 to colCount) yield document.createElement("td").asInstanceOf[html.Element]
    }
    for (pcm <- state.pcms) {
        for ((CellPosition(row, col), CellContent(color, stamp)) <- pcm.cellMap) {
          val td = tds(row)(col)
          def setText(text: String): Unit = {
            td.textContent = text
            td.style.color = color.toStyle
          }
          stamp match {
            case FillCellStamp => td.style.backgroundColor = color.toStyle
            case TextCellStamp(text) => setText(text)

            case      CrossCellStamp => setText("\u00d7")
            case        DotCellStamp => setText("\u00b7")
            case     CircleCellStamp => setText("\u25cb")
            case    UpArrowCellStamp => setText("\u2191")
            case  DownArrowCellStamp => setText("\u2193")
            case  LeftArrowCellStamp => setText("\u2190")
            case RightArrowCellStamp => setText("\u2192")
            case _ => ()
          }
        }
        for ((EdgePosition(row, col, ori), EdgeContent(color, stamp)) <- pcm.edgeMap) {
          stamp match {
            case NormalEdgeStamp => {
              val borderStyle = s"3px solid black"
              if (row < rowCount && col < colCount) {
                ori match {
                  case Horizontal => tds(row)(col).style.borderTop = borderStyle
                  case Vertical   => tds(row)(col).style.borderLeft = borderStyle
                }
              } else {
                ori match {
                  case Horizontal => tds(row-1)(col).style.borderBottom = borderStyle
                  case Vertical   => tds(row)(col-1).style.borderRight = borderStyle
                }
              }
            }
            case _ => ()
          }
        }
        // can't handle intersections, oh well
    }

    val table = document.createElement("table").asInstanceOf[html.Table]
    for (tdrow <- tds) {
      val tr = document.createElement("tr")
      for (td <- tdrow) {
        tr.appendChild(td)
      }
      table.appendChild(tr)
    }
    table
  }
}
