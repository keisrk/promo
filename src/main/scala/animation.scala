package promotion.animation

import scala.annotation.tailrec
abstract class Transition
case class ON2OFF(t: Double) extends Transition
case class OFF2ON(t: Double) extends Transition
case class ON(t: Double) extends Transition
case class OFF(t: Double) extends Transition

object TimeLine {
  @tailrec
  def s_of_assoc(acc: List[Transition], asc: List[Tuple2[Double, Boolean]]): List[Transition] = {
    asc match {
      case Nil => acc
      case (d, true)::xs => s_of_assoc((ON(d)::OFF2ON(0.5)::acc), xs)
      case (d, false)::xs => s_of_assoc((OFF(d)::ON2OFF(0.5)::acc), xs)
    }
  }
  def of_assoc(asc: List[Tuple2[Double, Boolean]]): List[Transition] = {
    s_of_assoc(List(), asc).reverse
  }
}
