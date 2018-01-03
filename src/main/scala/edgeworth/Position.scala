package edgeworth

sealed abstract class Position extends Ordered[Position] {
  def horizontalPosition: Int
  def verticalPosition: Int
  def deltaPosition(verticalDelta: Int, horizontalDelta: Int) =
    Position.getPosition(verticalPosition + verticalDelta,
        horizontalPosition + horizontalDelta)
  def roundToCell: CellPosition
  def roundToIntersection: IntersectionPosition
}
case class CellPosition(row: Int, col: Int) extends Position {
  def horizontalPosition = col * 2 + 1
  def verticalPosition = row * 2 + 1
  def compare(that: Position) = that match {
    case CellPosition(tr, tc) => {
      if (row != tr) row.compareTo(tr) else col.compareTo(tc)
    }
    case _ => -1
  }
  def roundToCell = this
  def roundToIntersection = new IntersectionPosition(row, col)
}
case class IntersectionPosition(row: Int, col: Int) extends Position {
  def horizontalPosition = col * 2
  def verticalPosition = row * 2
  def compare(that: Position) = that match {
    case IntersectionPosition(tr, tc) => {
      if (row != tr) row.compare(tr) else col.compare(tc)
    }
    case _ => 1
  }
  def roundToCell = new CellPosition(row, col)
  def roundToIntersection = this
}
sealed abstract class EdgeOrientation(val rowDelta: Int, val colDelta: Int)
case object Horizontal extends EdgeOrientation(0, 1)
case object Vertical extends EdgeOrientation(1, 0)

case class EdgePosition(row: Int, col: Int, orientation: EdgeOrientation) extends Position {
  def horizontalPosition = col * 2 + orientation.colDelta
  def verticalPosition = row * 2 + orientation.rowDelta
  def compare(that: Position) = that match {
    case EdgePosition(tr, tc, torient) => {
      (orientation, torient) match {
        case (Horizontal, Vertical) => -1
        case (Vertical, Horizontal) => 1
        case (Horizontal, Horizontal) => if (row != tr) row.compare(tr) else col.compare(tc)
        case (Vertical, Vertical) => if (col != tc) col.compare(tc) else row.compare(tr)
      }
    }
    case CellPosition(_, _) => 1
    case IntersectionPosition(_, _) => -1
  }
  def roundToCell = new CellPosition(row, col)
  def roundToIntersection = new IntersectionPosition(row, col)
}

object Position {
  private def flooredHalf(a: Int) = {
    if (a >= 0) a / 2 else (a-1) / 2
  }
  def getPosition(vertical: Int, horizontal: Int): Position = {
    val col = flooredHalf(horizontal)
    val row = flooredHalf(vertical)
    if (horizontal % 2 == 0) {
      if (vertical % 2 == 0) new IntersectionPosition(row, col)
      else new EdgePosition(row, col, Vertical)
    } else {
      if (vertical % 2 == 0) new EdgePosition(row, col, Horizontal)
      else new CellPosition(row, col)
    }
  }
}
object CellPosition {
  def grid(rowCount: Int, colCount: Int): Iterable[CellPosition] = {
    for (row <- 0 until rowCount; col <- 0 until colCount)
        yield CellPosition(row, col)
  }
}
