package edgeworth

import org.scalajs.dom.svg

sealed abstract class CellStamp {
  def renderCell(grid: SimpleGrid, cpos: CellPosition, color: Color): Seq[svg.Element]
}

case object FillCellStamp extends CellStamp {
  def renderCell(grid: SimpleGrid, cpos: CellPosition, color: Color) = {
    val CellPosition(row, col) = cpos
    Seq(SVGUtil.rect(grid.computeX(col), grid.computeY(row), grid.colWidth, grid.rowHeight, color.toStyle))
  }
}
case object DotCellStamp extends CellStamp {
  def renderCell(grid: SimpleGrid, cpos: CellPosition, color: Color) = {
    val (x, y) = grid.computePositionCenter(cpos)
    Seq(SVGUtil.circle(x, y, grid.eighth, color.toStyle))
  }
}
case object CrossCellStamp extends CellStamp {
  def renderCell(grid: SimpleGrid, cpos: CellPosition, color: Color) = {
    val CellPosition(row, col) = cpos
    val x = grid.computeX(col)
    val y = grid.computeY(row)
    val w = grid.colWidth
    val h = grid.rowHeight
    Seq(SVGUtil.x(x + w*0.25, y + h*0.25, x + w*0.75, y + h*0.75, 3, color.toStyle))
  }
}
case object CircleCellStamp extends CellStamp {
  def renderCell(grid: SimpleGrid, cpos: CellPosition, color: Color) = {
    val (x, y) = grid.computePositionCenter(cpos)
    Seq(SVGUtil.circle(x, y, 2.25 * grid.eighth, "transparent", color.toStyle, 3))
  }
}
case class TextCellStamp(text: String) extends CellStamp {
  def renderCell(grid: SimpleGrid, cpos: CellPosition, color: Color) = {
    val (x, y) = grid.computePositionCenter(cpos)
    val fontSize = (grid.colWidth min grid.rowHeight) * 2.0 / 3.0
    Seq(SVGUtil.text(x, y, text, fontSize.toString + "pt", color.toStyle))
  }
}
sealed abstract class ArrowCellStamp(dx: Int, dy: Int) extends CellStamp {
  def renderCell(grid: SimpleGrid, cpos: CellPosition, color: Color) = {
    val (x, y) = grid.computePositionCenter(cpos)
    SVGUtil.arrow(x, y, dx * grid.fourth, dy * grid.fourth, 4, color.toStyle)
  }
}
case object    UpArrowCellStamp extends ArrowCellStamp( 0, -1)
case object  DownArrowCellStamp extends ArrowCellStamp( 0,  1)
case object  LeftArrowCellStamp extends ArrowCellStamp(-1,  0)
case object RightArrowCellStamp extends ArrowCellStamp( 1,  0)

sealed abstract class LineCellStamp(dx: Int, dy: Int) extends CellStamp {
  def renderCell(grid: SimpleGrid, cpos: CellPosition, color: Color) = {
    val (x, y) = grid.computePositionCenter(cpos)
    val xdx = dx * grid.fourth
    val ydy = dy * grid.fourth
    Seq(SVGUtil.line(x - xdx, y - ydy, x + xdx, y + ydy, 4, color.toStyle))
  }
}

case object    HorizontalLineCellStamp extends LineCellStamp(1,  0)
case object      VerticalLineCellStamp extends LineCellStamp(0,  1)
case object  ForwardSlashLineCellStamp extends LineCellStamp(1,  1)
case object BackwardSlashLineCellStamp extends LineCellStamp(1, -1)

case object PlusCellStamp extends CellStamp {
  def renderCell(grid: SimpleGrid, cpos: CellPosition, color: Color) = {
    val (x, y) = grid.computePositionCenter(cpos)
    Seq(SVGUtil.plus(x, y, grid.fourth, grid.fourth, 4, color.toStyle))
  }
}

object CellStamp {
  def encode(stamp: CellStamp): String = stamp match {
    // cannot use "c" because that's for color
    case              FillCellStamp => "f"
    case               DotCellStamp => "d"
    case             CrossCellStamp => "x"
    case            CircleCellStamp => "o"
    case           UpArrowCellStamp => "au"
    case         DownArrowCellStamp => "ad"
    case         LeftArrowCellStamp => "al"
    case        RightArrowCellStamp => "ar"
    case    HorizontalLineCellStamp => "lh"
    case      VerticalLineCellStamp => "lv"
    case  ForwardSlashLineCellStamp => "lf"
    case BackwardSlashLineCellStamp => "lb"
    case              PlusCellStamp => "lp" // not technically a line but close enough
    case TextCellStamp(s) => TextCodec.encode('t', 'e', s)
  }
  def decode(s: StringIter): Option[CellStamp] = s.next() match {
    case Some('f') => Some(  FillCellStamp)
    case Some('d') => Some(   DotCellStamp)
    case Some('x') => Some( CrossCellStamp)
    case Some('o') => Some(CircleCellStamp)
    case Some('l') => s.next() match {
      case Some('h') => Some(   HorizontalLineCellStamp)
      case Some('v') => Some(     VerticalLineCellStamp)
      case Some('f') => Some( ForwardSlashLineCellStamp)
      case Some('b') => Some(BackwardSlashLineCellStamp)
      case Some('p') => Some(             PlusCellStamp)
      case Some(c) => {
        Out.warn("Cell stamp: Unrecognized line type: " ++ c.toString)
        None
      }
      case None => {
        Out.warn("Cell stamp: premature end at line")
        None
      }
    }
    case Some('a') => s.next() match {
      case Some('u') => Some(   UpArrowCellStamp)
      case Some('d') => Some( DownArrowCellStamp)
      case Some('l') => Some( LeftArrowCellStamp)
      case Some('r') => Some(RightArrowCellStamp)
      case Some(c) => {
        Out.warn("Cell stamp: Unrecognized arrow direction: " ++ c.toString)
        None
      }
      case None => {
        Out.warn("Cell stamp: premature end at arrow")
        None
      }
    }
    case Some('t') => Some(TextCellStamp(TextCodec.directDecode(s)))
    case Some('e') => Some(TextCellStamp(TextCodec.indirectDecode(s)))
    case Some(c) if TextCodec.singleDirectEncodeable(c) => Some(TextCellStamp(c.toString))
    case Some('.') => None
    case Some(c) => {
      Out.warn("Unrecognized character while parsing cell stamp: " ++ c.toString)
      None
    }
    case None => {
      Out.warn("Premature end while parsing cell stamp")
      None
    }
  }
}
