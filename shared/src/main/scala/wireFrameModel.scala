package sketch.wireframemodel

import sketch.matrix.{DblMat}
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
  def sort(el: List[Edge]): List[Edge]
  def toEdge(s: Shape): List[Edge]
  def reduce(el: List[Edge], m: DblMat.Mat): List[Edge]
  def toMatrix(trans: List[Trans], i: Double): DblMat.Mat 
  def dump(p: Point, v: View): (Double, Double)
  def moveTo(x: Double, y: Double): Unit 
  def lineTo(x: Double, y: Double): Unit     
  def beginPath(): Unit
  def strokePath(): Unit
  def fill(flag: Boolean): Unit
  def draw(es: List[Edge], v: View): Unit = {
    for (e <- es) {
      e match {
        case Nil => {}
        case p::tl => {
    beginPath()
          ((tp: (Double, Double))=> moveTo(tp._1, tp._2))(dump(p, v))
          for (q <- tl) {
            ((tp: (Double, Double))=> lineTo(tp._1, tp._2))(dump(q, v))
          }
          fill(true)
    strokePath()
        }
      }
    }
  }
}

abstract class P2D extends Prelude {
  type View = (Double, Double, Double)
  type Point = (Double, Double)
  def dump(x: Point, v: View): (Double, Double) = x
  def load(s: String) = Compose(None, List())
  def sort(el: List[Edge]): List[Edge] = el
  def toEdge(s: Shape): List[Edge] = s match {
    case Vec(id, o, (x, y)) => List(List(o, add(o, (x, 0d)), add(o, (x, y)), add(o, (0d, y)), o))
    case Poly(id, s, d) => d match {
      case None => List(s)
      case Some(z) => List(s)}
    case Compose(id, l) => l.foldLeft(List[Edge]()){(acc, e) => acc ++ toEdge(e)}
    case _ => List(List())
  }
  def toMatrix(trans: List[Trans], i: Double): DblMat.Mat = {//Not yet implemented
    DblMat.init(3, 3, ((i, j) => if (i == j) {1} else {0}))
  }
  def reduce(el: List[Edge], m: DblMat.Mat): List[Edge] = {el}
  def add(o: Point, d: Point): Point = (o, d) match {
    case ((ox, oy), (dx, dy)) => (ox + dx, oy + dy)
  }
}

abstract class P3D extends Prelude {
  type View = (Double, Double, Double)
  type Point = (Double, Double, Double)

  def toMatrix(trans: List[Trans], i: Double): DblMat.Mat = {
    val init = DblMat.init(4, 4, ((i, j) => if (i == j) {1} else {0}))
    trans.foldLeft(init){(acc, tr) => tr match {
      case Tr(o, d) => val (dx, dy, dz) = inter(o, d, i); DblMat.dotp(acc, DblMat.trans(dx, dy, dz), 4, 4)
      case Rx(o, d) => DblMat.dotp(acc, DblMat.rotx(o + (d - o) * i), 4, 4)
      case Ry(o, d) => DblMat.dotp(acc, DblMat.roty(o + (d - o) * i), 4, 4)
      case Rz(o, d) => DblMat.dotp(acc, DblMat.rotz(o + (d - o) * i), 4, 4)
    }}}

  def dump(p: Point, v: View): (Double, Double) = v match {
    case (rz, dx, dz) => p_reduce(p, DblMat.view(rz, dx, dz)) match {
      case (x, y, z) => (x + y * 0.5, z + y * 0.5)
  }}
  //def sort(el: List[Edge]): List[Edge] = el.sortWith(_.max() < _.max())
  def toEdge(s: Shape):List[Edge] = s match {
    case Vec(id, o, (x, y, z)) => List(/*
      List(o, add(o, (x, 0d, 0d)), add(o, (x, y, 0d)), add(o, (0d, y, 0d)), o,
        add(o, (0d, 0d, z)), add(o, (x, 0d, z)), add(o, (x, y, z)), add(o, (0d, y, z)), add(o, (0d, 0d, z))),
      List(add(o, (x, 0d, 0d)), add(o, (x, 0d, z))),
      List(add(o, (0d, y, 0d)), add(o, (0d, y, z))),
      List(add(o, (x, y, 0d)), add(o, (x, y, z)))*/
     List(add(o, (0d, 0d, z)), add(o, (x, 0d, z)), add(o, (x, y, z)), add(o, (0d, y, z)), add(o, (0d, 0d, z))),
     List(o, add(o, (x, 0d, 0d)), add(o, (x, 0d, z)), add(o, (0d, 0d, z)), o),
     List(add(o, (x, 0d, 0d)), add(o, (x, y, 0d)), add(o, (x, y, z)), add(o, (x, 0d, z)), add(o, (x, 0d, 0d)))
     )
    case Poly(id, s, d) => d match { 
      case None => List(s) 
      case Some(z) => {
        s.sliding(2).toList.map{case a::b::x => a::add(a, (0d, 0d, z))::add(b, (0d, 0d, z))::b::a::x}
        /*
  val head = s.head
        val last = s.last
        List(
          s ++ List(last, add(last, (0d, 0d, z))) ++ s.map{o => add(o, (0d, 0d, z))}.reverse ++ List(add(head, (0d, 0d, z)), head))
        */}//List(s) ++ List(s.map{o => add(o, (0d, 0d, z))}) ++ s.map{o => List(o, add(o, (0d, 0d, z)))}
    }
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
