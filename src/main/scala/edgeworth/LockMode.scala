package edgeworth

sealed abstract class LockMode {
  def apply(pos: Position): Position
  def multiplier: Int
}

case object NoLock extends LockMode {
  def apply(pos: Position) = pos
  val multiplier = 1
}
case object LockToCells extends LockMode {
  def apply(pos: Position) = pos.roundToCell
  val multiplier = 2
}
case object LockToIntersections extends LockMode {
  def apply(pos: Position) = pos.roundToIntersection
  val multiplier = 2
}
