package edgeworth

import org.scalajs.dom
import scala.scalajs.js

object Out {
  def log  (x: js.Any) = dom.console.log(x)
  def warn (x: js.Any) = dom.console.warn(x)
  def error(x: js.Any) = dom.console.error(x)
}
