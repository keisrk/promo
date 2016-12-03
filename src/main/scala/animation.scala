package promotion.animation

import promotion.wireframemodel.Edge3D
import promotion.data.Data

class Clock(val m: Int) {
  var t = 0d
  def incl(x: Double) : Unit = {
    t = (t + x) % m
  }

  def time(): Double = {
    t
  }
  def tl(ts: List[Double => List[Edge3D]]): List[Edge3D] = ts match {
    case Nil => Nil
    case f::fs => f(this.time()) ++ tl(fs)
  }
}
