package sketch.fxio3d

import sketch.matrix.{DblMat}
import sketch.wireframemodel.{P3D, Prelude}

import scalafx.scene.canvas.{GraphicsContext => Ctx2D}
import scala.util.parsing.json.JSON

abstract class FxOut(ctx: Ctx2D) extends P3D with Prelude{
  ctx.fillStyle = scalafx.scene.paint.Color.White
  def moveTo(x: Double, y: Double): Unit = ctx.moveTo(x, y)
  def lineTo(x: Double, y: Double): Unit = ctx.lineTo(x, y)
  def beginPath(): Unit = ctx.beginPath()
  def strokePath(): Unit = ctx.strokePath()
  def fill(flag: Boolean): Unit = if (flag) {ctx.fillPath()} else {}
}
class FxIO3D(ctx: Ctx2D) extends FxOut(ctx: Ctx2D) { 
  def v_load(id: Option[String], s: Any): Shape = if (s.isInstanceOf[List[List[Double]]]){
    val d = for (e <- s.asInstanceOf[List[List[Double]]]) yield (e(0), e(1), e(2))
      Vec(id, d(0), d(1))
  }else{
    sys.error("""s is not InstanceOf[List[List[Double]]]""")
  }
  def p_load(id: Option[String], s: Any, d: Option[Any]): Shape = if (s.isInstanceOf[List[List[Double]]]){
    val seq = for (e <- s.asInstanceOf[List[List[Double]]]) yield (e(0), e(1), e(2))
    val z = d match {
      case None => None
      case Some(v) => if (v.isInstanceOf[Double]) {Some(v.asInstanceOf[Double])} else {None}
    }
    Poly(id, seq.toList, z)
  }else{
    sys.error("""s is not InstanceOf[List[List[Double]]]""")
  }
  def s_load(s: List[Map[String, Any]]): List[Shape] = {
    s.map { c => c.get("shape").asInstanceOf[Option[String]] match {
      case None => sys.error("""c.get("shape") returns None""")
      case Some("vec") => c.get("data") match {
        case None => sys.error("""c.get("data") returns None""")
        case Some(d) => v_load(c.get("id").asInstanceOf[Option[String]], d)
      }
      case Some("ply") => (c.get("seq"), c.get("d")) match {
        case (Some(s), d) => p_load(c.get("id").asInstanceOf[Option[String]], s, d)
        case _ => sys.error("""c.get("seq") returns None""")
      }
      case Some("cmp") => c.get("data") match {
        case Some(d) => Compose(c.get("id").asInstanceOf[Option[String]], s_load(d.asInstanceOf[List[Map[String, Any]]]))
        case _ => sys.error("""c.get("data") returns None""")
      }
      case Some("cyl") => (c.get("seq"), c.get("d")) match {
        case (Some(s), d) => p_load(c.get("id").asInstanceOf[Option[String]], s, d)
        case _ => sys.error("""c.get("seq") returns None""")
      }}}.toList
  }
  def load(s: String): Shape = JSON.parseFull(s) match {
    case None => sys.error("""JSON.parseFull("data_string") returns None""")
    case Some(j) => Compose(Some("root"), s_load(j.asInstanceOf[List[Map[String, Any]]]))
  }
  def dispatch(k: String, o: Double, d: Double): Trans = k match {
    case "rx" => Rx(o, d)
    case "ry" => Ry(o, d)
    case "rz" => Rz(o, d)
    case _ => Rx(0d, 0d)
  }
  def ss_load_trans(src: Map[String, Any]): (String, Point, List[Trans]) = {
    val tr = src.asInstanceOf[Map[String, Any]]
    val n = tr.get("id").asInstanceOf[Option[String]] match {case None => "None"; case Some(n) => n}
    val o: Point = tr.get("off").asInstanceOf[Option[List[Double]]] match {case None => (0, 0, 0); case Some(a) => (a(0), a(1), a(2))}
    val ms = tr.map{
      case ("tr", v) => val a = v.asInstanceOf[List[List[Double]]]; Tr((a(0)(0), a(0)(1), a(0)(2)), (a(1)(0), a(1)(1), a(1)(2)))
      case (k, v) if (k == "rx" || k == "ry" || k == "rz") => val a = v.asInstanceOf[List[Double]]; dispatch(k, a(0), a(1))
      case _ => Rx(0d, 0d)
    }.toList
    (n, o, ms)
  }

  def load_trans(s: String): Map[String, (Point, List[Trans])] = JSON.parseFull(s) match {
    case None => sys.error("""JSON.parseFull("data_string") returns None""")
    case Some(j) => s_load_trans(j.asInstanceOf[List[Map[String, Any]]])
  }
  def s_load_trans(j: List[Map[String, Any]]): Map[String, (Point, List[Trans])] = {
    val l: List[(String, Point, List[Trans])] = j.map {c => ss_load_trans(c)}.toList
    Map(l.map{case (id, p, tr) => (id -> (p, tr))}: _*)
  }
  def mashup(shp: Shape, trans: Map[String, (Point, List[Trans])], st8: String, i: Double): List[Edge] = shp match {
    case base@(Vec(_, _, _)|Poly(_, _, _)) => trans.get(st8 + "-" + showId(base)) match {
      case Some((p, tr)) => reduce(toEdge(offset(p, base)), toMatrix(tr, i))
      case None => reduce(toEdge(base), DblMat.init(4, 4, ((i, j) => if (i == j) {1} else {0})))
    }
    case Compose(id, l) => trans.get(st8 + "-" + showId(Compose(id, l))) match {
      case Some((p, tr)) => reduce(addEl(p, l.foldLeft(List[Edge]()){ case (acc, indc) => mashup(indc, trans, st8, i):::acc}), toMatrix(tr, i))
      case None => l.foldLeft(List[Edge]()){ case (acc, indc) => mashup(indc, trans, st8, i):::acc}
    }
  }
}
