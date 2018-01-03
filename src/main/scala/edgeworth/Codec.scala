package edgeworth
import scala.collection.mutable.HashMap
import scala.scalajs.js
import org.scalajs.dom

// Fields (all separated by dots) are classified by first character.
// Digit: Grid size and genre.
// Uppercase letter: Grid data.
//
// safe URL chars are letters, digits, - . _ ~
// ?10x10slither.garbagedata
// First, compile an array of all CellContents.
// Some points of comparison:
// Slitherlink: blank + 0~3
// Nurikabe: majority blank + positive numbers
// Tapa: majority blank + multiple numbers per cell
// Akari: blank + 
// Edge puzzles:
// LITS, Star Battle: blank + filled
// Heyawacky: majority blank + nonnegative integers; blank + filled
//
// The "Edgeworth VM" takes variable-length instructions from 0 to 63.
// The VM should be initialized with a number n of possible non-blank contents,
// which is fixed for the parsing of a string (e.g. 4 for Slitherlink; infinity
// for Nurikabe et al). Then, instructions:
// - i = 0 to 15 (A to P): Set mode or something. Reserved ish for now.
// - i == 16: Insert content specified by base 32; 32+j are continuation bytes.
// - i == 17: Long skip (read + 1) * max(1, lo // n).
// - 32 <= i < 48: Skip (i // n) cells, place content (i % n).
// - 48 <= i < 64: Long skip ((j + 1) * max(1, lo // n))

class ContentCollection[T](var contents: Seq[T], var length: Int, backup: Option[(Int) => T] = None) {
  def +=(t: T): Unit = { contents :+= t }
  def apply(index: Int): T = contents.lift(index).getOrElse(backup.get(index))
}

object Codec {
  def decreasingFrequencyCollect[T](iter: Iterable[T]): Seq[T] = {
    var m = HashMap[T, Int]()
    for (t <- iter) {
      m(t) = m.getOrElse(t, 0) - 1
    }
    m.toSeq.sortBy(_._2).map(_._1)
  }
  def encodePart(len: Int, seq: Iterable[(Int, Int)]): String = {
    val longSkipLength = 1 max (16 / len)
    val ret = new StringBuffer()
    for ((dist, index) <- seq) yield {
      val longSkipAmount = dist / longSkipLength
      if (longSkipAmount >= 16) {
        ret.append(Base64.intToChar(17) + Base64.encodeBase32(longSkipAmount - 1))
      } else if (longSkipAmount > 0) {
        ret.append(Base64.intToChar(48 + longSkipAmount - 1))
      }
      val quick = dist % longSkipLength * len + index
      if (quick >= 16) {
        ret.append(Base64.intToChar(16) + Base64.encodeBase32(quick))
      } else {
        ret.append(Base64.intToChar(32 + quick))
      }
    }
    ret.toString
  }
  def optWrap(p: String, e: String, s: String) = if (s == "") "" else p ++ s ++ e
  def encode(gridDecorator: GridDecorator, gridBounds: GridBounds, pcm: PositionContentMap): String = {
    val prefix = s"${gridBounds.rowCount}x${gridBounds.colCount}."
    val decoratorEncoded = if (gridDecorator == GridDecorator.default) {
      ""
    } else {
      "d" ++ GridDecorator.encode(gridDecorator)
    }
    val ccSeq = decreasingFrequencyCollect(pcm.cellMap.values)
    val ccEncoded = optWrap("c", ".", ccSeq.map(CellContent.encode(_)).mkString(""))
    val encodedCells = {
      var last = CellPosition(0, 0)
      val entries = pcm.cellMap.toSeq.sortBy(_._1.asInstanceOf[Position])
      val pairs: Iterable[(Int, Int)] = for ((pos, cont) <- entries) yield {
        val dist = gridBounds.cellDistance(last, pos)
        last = gridBounds.skipped(pos, 1).asInstanceOf[CellPosition]
        (dist, ccSeq.indexOf(cont))
      }
      optWrap(Base64.intToChar(0).toString, "", encodePart(ccSeq.length, pairs))
    }

    val ecSeq = decreasingFrequencyCollect(pcm.edgeMap.values)
    val ecEncoded = optWrap("e", ".", ecSeq.map(EdgeContent.encode(_)).mkString(""))
    val encodedEdges = (for ((ori, startInst) <- Seq((Horizontal, 1), (Vertical, 2))) yield {
      var last = EdgePosition(0, 0, ori)
      val entries = pcm.edgeMap.toSeq.filter(_._1.orientation == ori).sortBy(_._1.asInstanceOf[Position])
      val pairs: Iterable[(Int, Int)] = for ((pos, cont) <- entries) yield {
        val dist = gridBounds.edgeDistance(last, pos)
        last = gridBounds.skipped(pos, 1).asInstanceOf[EdgePosition]
        (dist, ecSeq.indexOf(cont))
      }
      optWrap(Base64.intToChar(startInst).toString, "", encodePart(ecSeq.length, pairs))
    }).mkString("")

    val icSeq = decreasingFrequencyCollect(pcm.intersectionMap.values)
    val icEncoded = optWrap("i", ".", icSeq.map(IntersectionContent.encode(_)).mkString(""))
    val encodedIntersections = {
      var last = IntersectionPosition(0, 0)
      val entries = pcm.intersectionMap.toSeq.sortBy(_._1.asInstanceOf[Position])
      val pairs: Iterable[(Int, Int)] = for ((pos, cont) <- entries) yield {
        val dist = gridBounds.intersectionDistance(last, pos)
        last = gridBounds.skipped(pos, 1).asInstanceOf[IntersectionPosition]
        (dist, icSeq.indexOf(cont))
      }
      optWrap(Base64.intToChar(3).toString, "", encodePart(icSeq.length, pairs))
    }

    Seq(prefix, decoratorEncoded, ccEncoded, ecEncoded, icEncoded,
      encodedCells, encodedEdges, encodedIntersections).mkString("")
  }
  def untilNone[T](f: () => Option[T]): Seq[T] = {
    val ret = js.Array[T]()
    while (true) f() match {
      case Some(c) => ret.append(c)
      case None => return ret
    }
    throw new AssertionError("??? untilNone")
  }
  def decode(origText: String): (GridDecorator, GridBounds, PositionContentMap) = {
    var s = new StringIter(origText)
    var gridDecorator: GridDecorator = SolidDecorator
    var gridBounds: GridBounds = GridBounds(10, 10)
    var map = new PositionContentMap()
    var cellContents = new ContentCollection[CellContent](Seq(CellContent.default), 1, Some((i: Int) => CellContent(Color.pencil, TextCellStamp(i.toString))))
    var edgeContents = new ContentCollection[EdgeContent](Seq(EdgeContent.default), 1, None)
    var intersectionContents = new ContentCollection[IntersectionContent](Seq(IntersectionContent.default), 1, None)

    while (s.hasNext()) s.next().get match {
      case c if c.isDigit => {
        s.back()
        val dim1 = s.nextDigitsInt().toInt
        val genre = s.peek() match {
          case Some('x') => {
            s.next()
            gridBounds = GridBounds(dim1, s.nextDigitsInt())
            s.until('.')
          }
          case _ => {
            gridBounds = GridBounds(dim1, dim1)
            s.until('.')
          }
        }
        genre match {
          case "slitherlink" | "slither" | "sl" => {
            gridDecorator = DotDecorator
            cellContents = new ContentCollection[CellContent](
              (for (i <- 0 to 3) yield CellContent(Color.pencil, TextCellStamp(i.toString))).toSeq, 4, None)
          }
          case "" => ()
          case _ => Out.warn("unknown genre: " ++ genre)
        }
        // TODO: handle genre
      }
      case 'c' => {
        Out.log("Decoding cell contents")
        var cs = untilNone(() => CellContent.decode(s))
        if (!cs.nonEmpty) {
          Out.warn("Zero CellContents decoded!?")
          cs = Seq(CellContent.default)
        }
        cellContents = new ContentCollection[CellContent](cs, cs.length, Some((i: Int) => CellContent(Color.pencil, TextCellStamp(i.toString))))
        Out.log("Done decoding cell contents")
      }
      case 'e' => {
        Out.log("Decoding edge contents")
        var cs = untilNone(() => EdgeContent.decode(s))
        if (!cs.nonEmpty) {
          Out.warn("Zero EdgeContents decoded!?")
          cs = Seq(EdgeContent.default)
        }
        edgeContents = new ContentCollection[EdgeContent](cs, cs.length, None)
        Out.log("Done decoding edge contents")
      }
      case 'i' => {
        Out.log("Decoding intersection contents")
        var cs = untilNone(() => IntersectionContent.decode(s))
        if (!cs.nonEmpty) {
          Out.warn("Zero IntersectionContents decoded!?")
          cs = Seq(IntersectionContent.default)
        }
        intersectionContents = new ContentCollection[IntersectionContent](cs, cs.length, None)
        Out.log("Done decoding intersection contents")
      }
      case 'd' => {
        Out.log("Decoding decorator")
        gridDecorator = GridDecorator.decode(s)
        Out.log("Done decoding decorator")
      }
      case 'A' | 'B' | 'C' | 'D' => {
        s.back()
        Out.log("Starting VM")
        var position: Position = CellPosition(0, 0)
        def currentContentLength(): Int = position match {
          case cpos: CellPosition => cellContents.length
          case epos: EdgePosition => edgeContents.length
          case ipos: IntersectionPosition => intersectionContents.length
        }
        def skip(count: Int): Unit = {
          Out.log("Skipping " ++ count.toString)
          position = gridBounds.skipped(position, count)
        }
        def put(index: Int): Unit = {
          Out.log("Putting content " ++ index.toString)
          position match {
            case cpos: CellPosition => map.putCellContent(cpos, cellContents(index))
            case epos: EdgePosition => map.putEdgeContent(epos, edgeContents(index))
            case ipos: IntersectionPosition => map.putIntersectionContent(ipos, intersectionContents(index))
          }
          skip(1)
        }

        def putQuick(i: Int): Unit = {
          val len = currentContentLength()
          Out.log("Quick-putting " ++ i.toString)
          if (len == 0) {
            Out.error("Current content length is 0! This is really bad.")
          } else {
            skip(i / len); put(i % len)
          }
        }
        def skipLong(i: Int): Unit = {
          Out.log("Long-skipping " ++ i.toString)
          skip(i * (1 max (16 / currentContentLength())))
        }

        def readBase32(): Int = {
          Out.log("Reading base 32...")
          val n = Base64.decodeBase32(s)
          Out.log("Base 32 result: " ++ n.toString)
          n
        }
        var ch = s.next()
        while (ch != None && ch != Some('.')) {
          val instruction = Base64.charToInt(ch.get)
          Out.log("Instruction " ++ instruction.toString)
          if (32 <= instruction && instruction < 48) {
            putQuick(instruction - 32)
          } else if (48 <= instruction && instruction < 64) {
            skipLong(instruction - 48 + 1)
          } else instruction match {
            case 0 => {
              Out.log("VM to cells")
              position = CellPosition(0, 0)
            }
            case 1 => {
              Out.log("VM to horizontal edges")
              position = EdgePosition(0, 0, Horizontal)
            }
            case 2 => {
              Out.log("VM to vertical edges")
              position = EdgePosition(0, 0, Vertical)
            }
            case 3 => {
              Out.log("VM to intersections")
              position = IntersectionPosition(0, 0)
            }
            case 16 => put(readBase32())
            case 17 => skipLong(readBase32() + 1)
            case _ => Out.warn("in 'VM' cannot parse " ++ instruction.toString)
          }
          ch = s.next()
        }
        Out.log("ending VM")
      }
      case c => Out.warn("unrecognized thing " ++ c.toString)
    }
    (gridDecorator, gridBounds, map)
  }
}
