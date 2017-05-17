package promotion.animation

class Clock(val m: Int, val st8: List[String]) {
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
  def state(): String = {
    st8(index % st8.length)
  }
}
