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
  final case class Vec(o: Point, v: Point) extends Shape
  final case class Poly(s: List[Point]) extends Shape
  final case class Compose(l: List[Shape]) extends Shape
  def load(s: String): List[Shape]
  def dump(p: Point, v: View): (Double, Double)
  def toEdge(s: Shape, v: View): List[Edge]
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
  def load(s: String) = List()
  def toEdge(s: Shape, v: View): List[Edge] = s match {
    case Vec(o, (x, y)) => List(List(o, add(o, (x, 0d)), add(o, (x, y)), add(o, (0d, y)), o))
    case Poly(s) => List(s)
    case Compose(l) => l.foldLeft(List[Edge]()){(acc, e) => acc ++ toEdge(e, v)}
    case _ => List(List())
  }
  def add(o: Point, d: Point): Point = (o, d) match {
    case ((ox, oy), (dx, dy)) => (ox + dx, oy + dy)
  }
  val v = Vec((20d, 20d), (50d, 50d))
  val e = List((10d, 10d), (50d, 10d), (50d, 50d), (10d, 50d), (10d, 10d))
}

class P3D extends Prelude {
  type View = (Double, Double, Double, Double)
  type Point = (Double, Double, Double)

  def v_load(s: Dynamic): Shape = if (s.isInstanceOf[Array[Array[Double]]]){
    val d = for (e <- s.asInstanceOf[Array[Array[Double]]]) yield (e(0), e(1), e(2))
    Vec(d(0), d(1))
  }else{
    sys.error("""s is not InstanceOf[Array[Double]]""")
  }
  def p_load(s: Dynamic): Shape = if (s.isInstanceOf[Array[Array[Double]]]){
    val d = for (e <- s.asInstanceOf[Array[Array[Double]]]) yield (e(0), e(1), e(2))
    Poly(d.toList)
  }else{
    sys.error("""s is not InstanceOf[Array[Array[Double]]]""")
  }
  def s_load(s: Array[Dictionary[Dynamic]]): List[Shape] = {
    s.map { c => c.get("shape").asInstanceOf[Option[String]] match {
      case None => sys.error("""c.get("shape") returns None""")
      case Some("vec") => c.get("data") match {
        case None => sys.error("""c.get("data") returns None""")
        case Some(d) => v_load(d)
      }
      case Some("ply") => c.get("data") match {
        case None => sys.error("""c.get("data") returns None""")
        case Some(d) => p_load(d)
      }
    }}.toList
  }

  def load(s: String): List[Shape] = {
    val j = JSON.parse(s).asInstanceOf[Array[Dictionary[Dynamic]]]
    s_load(j)
  }
  def dump(p: Point, v: View): (Double, Double) = p match {
    case (x, y, z) => (x + z * 0.5, y + z * 0.5)
  }
  def toEdge(s: Shape, v: View):List[Edge] = s match {
    case Vec(o, (x, y, z)) => List(
      List(o, add(o, (x, 0d, 0d)), add(o, (x, y, 0d)), add(o, (0d, y, 0d)), o,
        add(o, (0d, 0d, z)), add(o, (x, 0d, z)), add(o, (x, y, z)), add(o, (0d, y, z)), add(o, (0d, 0d, z))),
      List(add(o, (x, 0d, 0d)), add(o, (x, 0d, z))),
      List(add(o, (0d, y, 0d)), add(o, (0d, y, z))),
      List(add(o, (x, y, 0d)), add(o, (x, y, z)))
    )
    case Poly(s) => List(s)
    case Compose(l) => l.foldLeft(List[Edge]()){(acc, e) => acc ++ toEdge(e, v)}
    case _ => List(List())
  }
  def add(o: Point, d: Point): Point = (o, d) match {
    case ((ox, oy, oz), (dx, dy, dz)) => (ox + dx, oy + dy, oz + dz)
  }
  def ofSeq(s: Seq[Double]): Point = (s(0), s(1), s(2))
  def toSeq(p: Point): Seq[Double] = p match {case (x, y, z) => Seq(x, y, z, 0)}
  def pmat(p: Point, m: DblMat.Mat): Point = ofSeq(DblMat.reduce(toSeq(p), m))
  val v = Vec((20d, 20d, 20d), (50d, 50d, 50d))
  val e = List(List((0d, 0d, 0d), (100d, 0d, 0d)), List((0d, 0d, 0d), (0d, 100d, 0d)), List((0d, 0d, 0d), (0d, 0d, 100d)))

  import scala.io.Source

/*
  val j = """[{"shape": "vec", "data": [[25, 20, 20], [90, 90, 90]]}, 
              {"shape": "ply", "data": [[0, 0, 0], [100, 0, 0], [0, 0, 0], [0, 100, 0], [0, 0, 0], [0, 0, 100]]},
              {"shape": "ply", "data": [[60, 60, 60], [120, 60, 60], [120, 120, 0], [60, 120, 60], [60, 60, 60]]}]"""*/
  //val jv = load(j)
}
