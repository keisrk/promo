package promotion.animation

import promotion.wireframemodelOld.Edge3D
import promotion.data.Data

class Clock(val m: Int) {
  var index = 0
  var t = 0d
  def incl(x: Double) : Unit = {
    t = (t + x) % m
    if (t == 0) { index += 1 }
  }

  def time(): Double = {
    t
  }
  def inter(): Double = t/m
  def state(q: List[String]): String = {
    q(index % q.length)
  }
  def tl(ts: List[Double => List[Edge3D]]): List[Edge3D] = ts match {
    case Nil => Nil
    case f::fs => f(this.time()) ++ tl(fs)
  }
}
