package edgeworth

case class GridBounds(rowCount: Int, colCount: Int) {
  def wrap(wrapLength: Int, maj: Int, min: Int): (Int, Int) = {
    (maj + min / wrapLength, min % wrapLength)
  }
  def cellDistance(p1: CellPosition, p2: CellPosition): Int = {
    val CellPosition(r1, c1) = p1
    val CellPosition(r2, c2) = p2
    (c2 - c1) + (r2 - r1) * colCount
  }
  def edgeDistance(p1: EdgePosition, p2: EdgePosition): Int = {
    val EdgePosition(r1, c1, o1) = p1
    val EdgePosition(r2, c2, o2) = p2
    (o1, o2) match {
      case (Horizontal, Horizontal) => (c2 - c1) + (r2 - r1) * colCount
      case (Vertical,   Vertical  ) => (r2 - r1) + (c2 - c1) * rowCount
      case _ => throw new IllegalArgumentException("edge distance between different orientations")
    }
  }
  def intersectionDistance(p1: IntersectionPosition, p2: IntersectionPosition): Int = {
    val IntersectionPosition(r1, c1) = p1
    val IntersectionPosition(r2, c2) = p2
    (c2 - c1) + (r2 - r1) * (colCount + 1)
  }
  def skipped(position: Position, count: Int): Position = position match {
    case CellPosition(row, col) => {
      val (r2, c2) = wrap(colCount, row, col + count)
      CellPosition(r2, c2)
    }
    case EdgePosition(row, col, Horizontal) => {
      val (r2, c2) = wrap(colCount, row, col + count)
      EdgePosition(r2, c2, Horizontal)
    }
    case EdgePosition(row, col, Vertical) => {
      val (c2, r2) = wrap(rowCount, col, row + count)
      EdgePosition(r2, c2, Vertical)
    }
    case IntersectionPosition(row, col) => {
      val (r2, c2) = wrap(colCount + 1, row, col + count)
      IntersectionPosition(r2, c2)
    }
  }
}
