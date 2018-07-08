package edgeworth

sealed abstract class Genre {
  def decorator: GridDecorator

  def puzzleCellContents: Seq[CellContent]
  def solutionEdgeContents: Seq[EdgeContent]

  def check(puzzle: PositionContentMap, solution: PositionContentMap): Option[String]
}

object Slitherlink extends Genre {
  val decorator = DotDecorator

  def puzzleCellContents = for (i <- 0 to 3) yield CellContent.text(i.toString)
  def solutionEdgeContents = Seq(EdgeContent(Color.pencil, NormalEdgeStamp), EdgeContent(Color.pencil, CrossEdgeStamp))

  def check(puzzle: PositionContentMap, solution: PositionContentMap): Option[String] = {
    Some("Checking not implemented yet!")
  }
}
