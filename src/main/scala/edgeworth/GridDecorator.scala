package edgeworth

// Represents the background
// Examples: Nurikabe might have a solid box and solid grid lines.
// Slitherlinks might have intersections.
// Fillominos might have a solid box and dashed grid lines.
// Corrals might have entirely dashed grid lines.
sealed abstract class GridDecorator {
  def decorate(master: MasterSVG, grid: SimpleGrid, gridBounds: GridBounds): Unit
  def id: String
  def description: String
}
case object SolidDecorator extends GridDecorator {
  def decorate(master: MasterSVG, grid: SimpleGrid, gridBounds: GridBounds): Unit = {
    master.clearDecoration()
    master.decorateLines(grid, gridBounds, "black", 1)
    master.decorateBorder(grid, gridBounds)
  }
  def id = "solid"
  def description = "Solid + border"
}
case object DashedDecorator extends GridDecorator {
  def decorate(master: MasterSVG, grid: SimpleGrid, gridBounds: GridBounds): Unit = {
    master.clearDecoration()
    master.decorateLines(grid, gridBounds, "gray", 1, "", 0.02)
    master.decorateLines(grid, gridBounds, "gray", 1, "2,2")
    master.decorateBorder(grid, gridBounds)
  }
  def id = "dashed"
  def description = "Dashed + border"
}
case object AllDashedDecorator extends GridDecorator {
  def decorate(master: MasterSVG, grid: SimpleGrid, gridBounds: GridBounds): Unit = {
    master.clearDecoration()
    master.decorateLines(grid, gridBounds, "gray", 1, "", 0.02, true)
    master.decorateLines(grid, gridBounds, "gray", 1, "2,2", 1.0, true)
  }
  def id = "alldashed"
  def description = "All dashed"
}
case object DotDecorator extends GridDecorator {
  def decorate(master: MasterSVG, grid: SimpleGrid, gridBounds: GridBounds): Unit = {
    master.clearDecoration()
    master.decorateLines(grid, gridBounds, "gray", 6, "", 0.02, true)
    master.decorateIntersections(grid, gridBounds)
  }
  def id = "dotgrid"
  def description = "Dot grid"
}

object GridDecorator {
  def allDecorators = Seq(SolidDecorator, DashedDecorator, AllDashedDecorator, DotDecorator)
  def default = SolidDecorator
  def encode(d: GridDecorator): String = d match {
    case SolidDecorator => "s"
    case DashedDecorator => "d"
    case AllDashedDecorator => "a"
    case DotDecorator => "o"
  }
  def decode(s: StringIter): GridDecorator = s.next() match {
    case Some('s') =>     SolidDecorator
    case Some('d') =>    DashedDecorator
    case Some('a') => AllDashedDecorator
    case Some('o') =>       DotDecorator
    case None => {
      Out.warn("premature end while parsing GridDecorator")
      SolidDecorator
    }
    case Some(c) => {
      Out.warn("unknown GridDecorator " ++ c.toString)
      SolidDecorator
    }
  }
}
