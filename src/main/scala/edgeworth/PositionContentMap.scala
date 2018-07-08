package edgeworth

class PositionContentMap(
  var cellMap: Map[CellPosition, CellContent] = Map(),
  var edgeMap: Map[EdgePosition, EdgeContent] = Map(),
  var intersectionMap: Map[IntersectionPosition, IntersectionContent] = Map()) {

  def putCellContent(cpos: CellPosition, cc: CellContent) = {
    cellMap += cpos -> cc
  }
  def putEdgeContent(epos: EdgePosition, ec: EdgeContent) = {
    edgeMap += epos -> ec
  }
  def putIntersectionContent(ipos: IntersectionPosition, ic: IntersectionContent) = {
    intersectionMap += ipos -> ic
  }
  def clearCellContent(cpos: CellPosition) = {
    cellMap -= cpos
  }
  def clearEdgeContent(epos: EdgePosition) = {
    edgeMap -= epos
  }
  def clearIntersectionContent(ipos: IntersectionPosition) = {
    intersectionMap -= ipos
  }
  def clear() = {
    cellMap = Map()
    edgeMap = Map()
    intersectionMap = Map()
  }
}
