package promotion.jsio3d

import org.scalajs.dom.{CanvasRenderingContext2D => Ctx2D}
import scala.scalajs.js.{Array, Dynamic, JSON}
import scala.scalajs.js.Dictionary
import promotion.matrix.{DblMat}
import promotion.wireframemodel.{P3D}

class JSIO3D extends P3D {
  def draw(ctx: Ctx2D, es: List[Edge], v: View): Unit = {
    ctx.beginPath()
    for (e <- es) {
      e match {
        case Nil => {}
	case p::tl => {
          ((tp: (Double, Double))=> ctx.moveTo(tp._1, tp._2))(dump(p, v))
	  for (q <- tl) {
          ((tp: (Double, Double))=> ctx.lineTo(tp._1, tp._2))(dump(q, v))
	  }
	}
      }
    }
    ctx.stroke()
  }
  def v_load(id: Option[String], s: Dynamic): Shape = if (s.isInstanceOf[Array[Array[Double]]]){
    val d = for (e <- s.asInstanceOf[Array[Array[Double]]]) yield (e(0), e(1), e(2))
      Vec(id, d(0), d(1))
  }else{
    sys.error("""s is not InstanceOf[Array[Double]]""")
  }
  def p_load(id: Option[String], s: Dynamic, d: Option[Dynamic]): Shape = if (s.isInstanceOf[Array[Array[Double]]]){
    val seq = for (e <- s.asInstanceOf[Array[Array[Double]]]) yield (e(0), e(1), e(2))
    val z = d match {
      case None => None
      case Some(v) => if (v.isInstanceOf[Double]) {Some(v.asInstanceOf[Double])} else {None}
    }
    Poly(id, seq.toList, z)
  }else{
    sys.error("""s is not InstanceOf[Array[Array[Double]]]""")
  }
  def s_load(s: Array[Dictionary[Dynamic]]): List[Shape] = {
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
        case Some(d) => Compose(c.get("id").asInstanceOf[Option[String]], s_load(d.asInstanceOf[Array[Dictionary[Dynamic]]]))
        case _ => sys.error("""c.get("data") returns None""")
      }
      case Some("cyl") => (c.get("seq"), c.get("d")) match {
        case (Some(s), d) => p_load(c.get("id").asInstanceOf[Option[String]], s, d)
        case _ => sys.error("""c.get("seq") returns None""")
      }}}.toList
  }

  def load(s: String): Shape = {
    val j = JSON.parse(s).asInstanceOf[Array[Dictionary[Dynamic]]]
    Compose(Some("root"), s_load(j))
  }
  def dispatch(k: String, o: Double, d: Double): Trans = k match {
    case "rx" => Rx(o, d)
    case "ry" => Ry(o, d)
    case "rz" => Rz(o, d)
    case _ => Rx(0d, 0d)
  }
  def tr_load(src: Dictionary[Dynamic]): (String, Point, List[Trans]) = {
    val tr = src.asInstanceOf[Dictionary[Dynamic]]
    val n = tr.get("id").asInstanceOf[Option[String]] match {case None => "None"; case Some(n) => n}
    val o: Point = tr.get("off").asInstanceOf[Option[Array[Double]]] match {case None => (0, 0, 0); case Some(a) => (a(0), a(1), a(2))}
    val ms = tr.map{
      case ("tr", v) => val a = v.asInstanceOf[Array[Array[Double]]]; Tr((a(0)(0), a(0)(1), a(0)(2)), (a(1)(0), a(1)(1), a(1)(2)))
      case (k, v) if (k == "rx" || k == "ry" || k == "rz") => val a = v.asInstanceOf[Array[Double]]; dispatch(k, a(0), a(1))
      case _ => Rx(0d, 0d)
    }.toList
    (n, o, ms)
  }

  def load_trs(s: String): Dictionary[(Point, List[Trans])] = {
    val j = JSON.parse(s).asInstanceOf[Array[Dictionary[Dynamic]]]
    s_load_trs(j)
  }
  def s_load_trs(j: Array[Dictionary[Dynamic]]): Dictionary[(Point, List[Trans])] = {
    val l: List[(String, Point, List[Trans])] = j.map {c => tr_load(c)}.toList
    Dictionary(l.map{case (id, p, tr) => (id -> (p, tr))}: _*)
  }
  def mashup(shp: Shape, trs: Dictionary[(Point, List[Trans])], st8: String, i: Double): List[Edge] = shp match {
    case base@(Vec(_, _, _)|Poly(_, _, _)) => trs.get(st8 + "-" + showId(base)) match {
      case Some((p, tr)) => reduce(toEdge(offset(p, base)), toMatrix(tr, i))
      case None => reduce(toEdge(base), DblMat.init(4, 4, ((i, j) => if (i == j) {1} else {0})))
    }
    case Compose(id, l) => trs.get(st8 + "-" + showId(Compose(id, l))) match {
      case Some((p, tr)) => reduce(addEl(p, l.foldLeft(List[Edge]()){ case (acc, indc) => mashup(indc, trs, st8, i):::acc}), toMatrix(tr, i))
      case None => l.foldLeft(List[Edge]()){ case (acc, indc) => mashup(indc, trs, st8, i):::acc}
    }
  }
}


