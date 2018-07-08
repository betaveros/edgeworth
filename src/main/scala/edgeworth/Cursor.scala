package edgeworth

class Cursor(initselected: Option[Position] = None) {
  var selected: Option[Position] = initselected

  def computePath(grid: SimpleGrid): String = {
    selected match {
      case Some(CellPosition(row, col)) => {
        val x  = grid.computeX(col)
        val y  = grid.computeY(row)
        val x2 = grid.computeX(col + 1)
        val y2 = grid.computeY(row + 1)
        s"M${x},${y}H${x2}V${y2}H${x}Z"
      }
      case Some(EdgePosition(row, col, Horizontal)) => {
        val xlo  = grid.computeX(col)
        val xhi  = grid.computeX(col + 1)
        val xmid = (xlo + xhi) / 2
        val y    = grid.computeY(row)
        val ylo  = y - 10
        val yhi  = y + 10
        s"M${xlo},${y}Q${xmid},${ylo} ${xhi},${y}Q${xmid},${yhi} ${xlo},${y}Z"
      }
      case Some(EdgePosition(row, col, Vertical)) => {
        val x    = grid.computeX(col)
        val xlo  = x - 10
        val xhi  = x + 10
        val ylo  = grid.computeY(row)
        val yhi  = grid.computeY(row + 1)
        val ymid = (ylo + yhi) / 2
        s"M${x},${ylo}Q${xlo},${ymid} ${x},${yhi}Q${xhi},${ymid} ${x},${ylo}Z"
      }
      case Some(IntersectionPosition(row, col)) => {
        val x  = grid.computeX(col)
        val y  = grid.computeY(row)
        s"M${x-5},${y-5}H${x+5}V${y+5}H${x-5}Z"
      }
      case None => ""
    }
  }
  def moveSelected(verticalDelta: Int, horizontalDelta: Int){
    selected = selected map
      {_.deltaPosition(verticalDelta, horizontalDelta)}
  }
  def shortStatus: String = {
    selected match {
      case Some(CellPosition(row, col)) => s"R${row+1}C${col+1}"
      case Some(EdgePosition(row, col, Horizontal)) => s"R${row}-${row+1}C${col+1}"
      case Some(EdgePosition(row, col, Vertical)) => s"R${row+1}C${col}-${col+1}"
      case Some(IntersectionPosition(row, col)) => s"R${row}-${row+1}C${col}-${col+1}"
      case None => ""
    }
  }
}
