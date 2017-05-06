package promotion.wireframemodel

import scala.scalajs.js.{Any, Array, Dynamic, JSON, Function2, Object}
import scala.scalajs.js.Dictionary
import org.scalajs.dom.{CanvasRenderingContext2D => Ctx2D}
import promotion.matrix.{DblMat}

trait Prelude {
  type View
  type Point
  type Edge = List[Point]
  sealed trait Shape
  final case class Vec(id: Option[String], o: Point, v: Point) extends Shape
  final case class Poly(id: Option[String], s: List[Point], d: Option[Double]) extends Shape
  final case class Compose(id: Option[String], l: List[Shape]) extends Shape

  sealed trait Trans
  final case class Tr(o: Point, d: Point) extends Trans
  final case class Rx(o: Double, d: Double) extends Trans
  final case class Ry(o: Double, d: Double) extends Trans
  final case class Rz(o: Double, d: Double) extends Trans
  def showId(s: Shape): String = s match {
    case Vec(id, _, _) => id match {case Some(n) => n; case None => ""}
    case Poly(id, _, _) => id match {case Some(n) => n; case None => ""}
    case Compose(id, _) => id match {case Some(n) => n; case None => ""}
  }
  def add(p: Point, q: Point): Point
  def addEl(p: Point, el: List[Edge]): List[Edge] = for (e <- el) yield ( for (q <- e) yield (add(p, q)))
  def offset(p: Point, s: Shape): Shape = s match {
    case Vec(id, o, v) => Vec(id, add(o, p), v)
    case Poly(id, s, d) => Poly(id, s.map( o => add(o, p)), d)
    case Compose(id, l) => Compose(id, l.map( t => offset(p, t)))
  }
  def load(s: String): Shape
  def dump(p: Point, v: View): (Double, Double)
  def toEdge(s: Shape): List[Edge]
  def reduce(el: List[Edge], m: DblMat.Mat): List[Edge]
  def toMatrix(trs: List[Trans], i: Double): DblMat.Mat 
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
}

class P2D extends Prelude {
  type View = (Double, Double, Double)
  type Point = (Double, Double)
  def dump(x: Point, v: View): (Double, Double) = x
  def load(s: String) = Compose(None, List())
  def toEdge(s: Shape): List[Edge] = s match {
    case Vec(id, o, (x, y)) => List(List(o, add(o, (x, 0d)), add(o, (x, y)), add(o, (0d, y)), o))
    case Poly(id, s, d) => d match {
      case None => List(s)
      case Some(z) => List(s)}
    case Compose(id, l) => l.foldLeft(List[Edge]()){(acc, e) => acc ++ toEdge(e)}
    case _ => List(List())
  }
  def toMatrix(trs: List[Trans], i: Double): DblMat.Mat = {//Not yet implemented
    DblMat.init(3, 3, ((i, j) => if (i == j) {1} else {0}))
  }
  def reduce(el: List[Edge], m: DblMat.Mat): List[Edge] = {el}
  def add(o: Point, d: Point): Point = (o, d) match {
    case ((ox, oy), (dx, dy)) => (ox + dx, oy + dy)
  }
}

class P3D extends Prelude {
  type View = (Double, Double, Double)
  type Point = (Double, Double, Double)

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
        case _ => sys.error("""c.get("data") returns None""")}
    }}.toList
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
  def toMatrix(trs: List[Trans], i: Double): DblMat.Mat = {
    val init = DblMat.init(4, 4, ((i, j) => if (i == j) {1} else {0}))
    trs.foldLeft(init){(acc, tr) => tr match {
      case Tr(o, d) => val (dx, dy, dz) = inter(o, d, i); DblMat.dotp(acc, DblMat.trans(dx, dy, dz), 4, 4)
      case Rx(o, d) => DblMat.dotp(acc, DblMat.rotx(o + (d - o) * i), 4, 4)
      case Ry(o, d) => DblMat.dotp(acc, DblMat.roty(o + (d - o) * i), 4, 4)
      case Rz(o, d) => DblMat.dotp(acc, DblMat.rotz(o + (d - o) * i), 4, 4)
    }}}

  def dump(p: Point, v: View): (Double, Double) = v match {
    case (rz, dx, dz) => p_reduce(p, DblMat.view(rz, dx, dz)) match {
      case (x, y, z) => (x + y * 0.5, z + y * 0.5)
  }}
  def toEdge(s: Shape):List[Edge] = s match {
    case Vec(id, o, (x, y, z)) => List(
      List(o, add(o, (x, 0d, 0d)), add(o, (x, y, 0d)), add(o, (0d, y, 0d)), o,
        add(o, (0d, 0d, z)), add(o, (x, 0d, z)), add(o, (x, y, z)), add(o, (0d, y, z)), add(o, (0d, 0d, z))),
      List(add(o, (x, 0d, 0d)), add(o, (x, 0d, z))),
      List(add(o, (0d, y, 0d)), add(o, (0d, y, z))),
      List(add(o, (x, y, 0d)), add(o, (x, y, z)))
    )
    case Poly(id, s, d) => d match { case None => List(s); case Some(z) => List(s) ++ List(s.map{o => add(o, (0d, 0d, z))}) ++ s.map{o => List(o, add(o, (0d, 0d, z)))}}
    case Compose(id, l) => l.foldLeft(List[Edge]()){(acc, e) => acc ++ toEdge(e)}
    case _ => List(List())
  }

  def add(o: Point, d: Point): Point = (o, d) match {
    case ((ox, oy, oz), (dx, dy, dz)) => (ox + dx, oy + dy, oz + dz)
  }
  def subt(o: Point, d: Point): Point = (o, d) match {
    case ((ox, oy, oz), (dx, dy, dz)) => (ox - dx, oy - dy, oz - dz)
  }
  def mult(p: Point, i: Double): Point = p match {
    case (px, py, pz) => (px * i, py * i, pz * i)
  }
  def inter(o: Point, d: Point, i: Double): Point = {
    val base = subt(d, o)
    add(o, mult(base, i))
  }
  def ofSeq(s: Seq[Double]): Point = (s(0), s(1), s(2))
  def toSeq(p: Point): Seq[Double] = p match {case (x, y, z) => Seq(x, y, z, 1)}
  def p_reduce(p: Point, m: DblMat.Mat): Point = ofSeq(DblMat.reduce(m, toSeq(p)))
  def e_reduce(e: Edge, m: DblMat.Mat): Edge = e.map(p => p_reduce(p, m))
  def reduce(el: List[Edge], m: DblMat.Mat): List[Edge] = el.map(e => e_reduce(e, m))
  def trans(el: List[Edge], dx: Double, dy: Double, dz: Double): List[Edge] = reduce(el, DblMat.trans(dx, dy, dz))
  def rotx(el: List[Edge], rx: Double): List[Edge] = reduce(el, DblMat.rotx(rx))
  def roty(el: List[Edge], ry: Double): List[Edge] = reduce(el, DblMat.roty(ry))
  def rotz(el: List[Edge], rz: Double): List[Edge] = reduce(el, DblMat.rotz(rz))
}
