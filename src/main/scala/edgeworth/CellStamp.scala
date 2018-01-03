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
    Seq(SVGUtil.text(x, y, text, fontSize.toString + "pt"))
  }
}
object CellStamp {
  def encode(stamp: CellStamp): String = stamp match {
    // cannot use "c" because that's for color
    case FillCellStamp => "f"
    case DotCellStamp => "d"
    case CrossCellStamp => "x"
    case CircleCellStamp => "o"
    case TextCellStamp(s) => TextCodec.directEncode(s) match {
      case Some(x) => (if (TextCodec.hasSafeHead(x)) "" else "t") ++ x
      case None => throw new AssertionError("Can't encode arbitrary text yet, sorry!")
    }
  }
  def decode(s: StringIter): Option[CellStamp] = s.next() match {
    case Some('f') => Some(  FillCellStamp)
    case Some('d') => Some(   DotCellStamp)
    case Some('x') => Some( CrossCellStamp)
    case Some('o') => Some(CircleCellStamp)
    case Some('t') => Some(TextCellStamp(TextCodec.directDecode(s)))
    case Some(c) if TextCodec.upperOrDigit(c) => Some(TextCellStamp(c.toString))
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
