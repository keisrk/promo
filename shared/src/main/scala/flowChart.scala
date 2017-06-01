package sketch.flowChart
class FlowChart(val init: String, val data: List[(String, String, Int)]) {
  var present = init
  var index = 0
  val g = Map(data.map{case (q, p, lbl) => (q -> (p, lbl))}: _*)
  val st8 = (x: Int) => if (x == index) { 
    present
  }else{ 
    index = x
    val p = present
    present = g(present)._1
    p
  }
}
