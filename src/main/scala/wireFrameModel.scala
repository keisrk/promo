package promotion.wireframemodel

import org.scalajs.dom.{CanvasRenderingContext2D => Ctx2D}
import promotion.matrix

trait Prelude {
  type Window
  type Point
  def dump(p: Point, w: Window): (Double, Double)
  type Edge = List[Point]
  sealed trait Shape
  final case class Vec(v: Point) extends Shape
  final case class Circ(r: Double) extends Shape
  final case class Other(e: Edge) extends Shape
  def load(s: String): List[Shape]

  def toEdge(s: Shape, o: Point, w: Window): List[Edge]
  def draw(ctx: Ctx2D, es: List[Edge], w: Window): Unit = {
    ctx.beginPath()
    for (e <- es) {
      e match {
        case Nil => {}
	case p::tl => {
          ((tp: (Double, Double))=> ctx.moveTo(tp._1, tp._2))(dump(p, w))
	  for (q <- tl) {
          ((tp: (Double, Double))=> ctx.lineTo(tp._1, tp._2))(dump(q, w))
	  }
	}
      }
    }
    ctx.stroke()
  }
}

class P2D extends Prelude {
  type Window = (Double, Double, Double)
  type Point = (Double, Double)
  def dump(x: Point, w: Window): (Double, Double) = x
  def load(s: String) = List()
  def toEdge(s: Shape, o: Point, w: Window): List[Edge] = s match {
    case Vec((x, y)) => List(List(o, add(o, (x, 0d)), add(o, (x, y)), add(o, (0d, y)), o))
    case _ => List(List())
  }
  def add(o: Point, d: Point): Point = (o, d) match {
    case ((ox, oy), (dx, dy)) => (ox + dx, oy + dy)
  }
  val v = Vec((50d, 50d))
  val e = List((10d, 10d), (50d, 10d), (50d, 50d), (10d, 50d), (10d, 10d))
}

class P3D extends Prelude {
  type Window = (Double, Double, Double, Double)
  type Point = (Double, Double, Double)
  def dump(x: Point, w: Window) = (x._1, x._1)
  def load(s: String) = List()
  def project(p: Point, rot: Double): (Double, Double) = p match {
    case (x, y, z) => {
      val nx = Math.cos(Math.toRadians(rot)) * x - Math.sin(Math.toRadians(rot)) * z
      val ny = y
      val nz = Math.sin(Math.toRadians(rot)) * x + Math.cos(Math.toRadians(rot)) * z
      val show = nz * 0.5
      (nx + show, ny + show)
    }
  }
  def toEdge(s: Shape, o: Point, w: Window):List[Edge] = s match {
    case Vec((x, y, z)) => List(
      List(o, add(o, (x, 0d, 0d)), add(o, (x, y, 0d)), add(o, (x, y, 0d)), o)
      )
    case _ => List(List())
  }
  def add(o: Point, d: Point): Point = (o, d) match {
    case ((ox, oy, oz), (dx, dy, dz)) => (ox + dx, oy + dy, oz + dz)
  }
}