package promotion.wireframemodel

import org.scalajs.dom.{CanvasRenderingContext2D => Ctx2D}

class Window(val r: Double, val x: Double, val y: Double)
class Point2D(val x: Double, val y: Double){
}
class Point3D(val x: Double, val y: Double, val z: Double){
  def xpls(i: Double): Point3D = {
    new Point3D(x + i, y, z)
  }
  def ypls(i: Double): Point3D = {
    new Point3D(x, y + i, z)
  }
  def zpls(i: Double): Point3D = {
    new Point3D(x, y, z + i)
  }
}
class Edge2D(val p2: List[Point2D])
class Edge3D(val p3: List[Point3D])

class EdgeD2D(val org: Point2D, val dst: Point2D)
class EdgeD3D(val org: Point3D, val dst: Point3D)

abstract class Orient
case class XY(r: Double) extends Orient
case class YZ(r: Double) extends Orient

class Rect(val o: Point3D, val w: Double, val h: Double, val d: Double) {
  def toEdge3D(): List[Edge3D] = {
    val x = new Point3D(o.x + w, o.y, o.z)
    val y = new Point3D(o.x, o.y + h, o.z)
    val z = new Point3D(o.x, o.y, o.z + d)
    val xy = new Point3D(o.x + w, o.y + h, o.z)
    val yz = new Point3D(o.x, o.y + h, o.z + d)
    val zx = new Point3D(o.x + w, o.y, o.z + d)
    val xyz = new Point3D(o.x + w, o.y + h, o.z + d)
    List(new Edge3D(List(o, x, zx, xyz, yz, y, o, z, yz)), new Edge3D(List(z, zx)), new Edge3D(List(x, xy, xyz)), new Edge3D(List(y, xy)))
  }
  def toInt(x: Boolean): Integer = {
    if (x) 1 else 0
  }
  def vrtx(x: Boolean, y: Boolean, z: Boolean): Point3D = {
    new Point3D(o.x + w * toInt(x), o.y + h * toInt(y), o.z + d * toInt(z))
  }
}

class Poly(val o: Point3D, val r: Double, val d: Double, val n: Integer){
  val th = 360d/n.toDouble
  def hcos(th: Double, i: Double, r: Double): Double = Math.cos(Math.toRadians(th * i)) * r
  def hsin(th: Double, i: Double, r: Double): Double = Math.sin(Math.toRadians(th * i)) * r

  def toInt(x: Boolean): Integer = {
    if (x) 1 else 0
  }

  def toEdge3D(): List[Edge3D] = {
    val circ = for (j <- 0 to 1) yield {
      val ps = for (i <- ((0 to n - 1) :+ 0)) yield {
        new Point3D(o.x + hcos(th, i, r), o.y + hsin(th, i, r), o.z + (d * j))
      }
      new Edge3D(ps.toList)
    }
    val edge = for (i <- 0 to n - 1) yield {
      val ps = for (j <- 0 to 1) yield {
        new Point3D(o.x + hcos(th, i, r), o.y + hsin(th, i, r), o.z + (d * j))
      }
      new Edge3D(ps.toList)
    }
    circ.toList ++ edge.toList
  }

  def vrtx(isBack: Boolean, i: Integer): Point3D = {
    new Point3D(o.x + hcos(th, i.toDouble, r), o.y + hsin(th, i.toDouble, r), o.z + (d * toInt(isBack)))
  }
}

object WireFrameModel {
  def makeEdgeD3D(ls: List[Tuple2[Tuple3[Double, Double, Double], Tuple3[Double, Double, Double]]]): List[EdgeD3D] = {
    for (((ox, oy, oz), (dx, dy, dz)) <- ls) yield {
      val org = new Point3D(ox, oy, oz)
      val dst = new Point3D(dx, dy, dz)
      new EdgeD3D(org, dst)
    }
  }

  def makeEdge3D(ls: List[List[Tuple3[Double, Double, Double]]]): List[Edge3D] = {
    for (l <- ls) yield {
      new Edge3D(for ((x, y, z) <- l) yield {new Point3D(x, y, z)})
    }
  }

  def makeRect(o: Point3D, w: Double, h: Double, d: Double): List[Edge3D] = {
    val x = new Point3D(o.x + w, o.y, o.z)
    val y = new Point3D(o.x, o.y + h, o.z)
    val z = new Point3D(o.x, o.y, o.z + d)
    val xy = new Point3D(o.x + w, o.y + h, o.z)
    val yz = new Point3D(o.x, o.y + h, o.z + d)
    val zx = new Point3D(o.x + w, o.y, o.z + d)
    val xyz = new Point3D(o.x + w, o.y + h, o.z + d)
    List(new Edge3D(List(o, x, zx, xyz, yz, y, o, z, yz)), new Edge3D(List(z, zx)), new Edge3D(List(x, xy, xyz)), new Edge3D(List(y, xy)))
  }

  def hcos_old(i: Double): Double = Math.cos(Math.toRadians(60 * i)) * 12d
  def hsin_old(i: Double): Double = Math.sin(Math.toRadians(60 * i)) * 12d

  def hcos(th: Double, i: Double, r: Double): Double = Math.cos(Math.toRadians(th * i)) * r
  def hsin(th: Double, i: Double, r: Double): Double = Math.sin(Math.toRadians(th * i)) * r

  def makePoly(o: Point3D, r: Double, d: Double, n: Integer): List[Edge3D] = {
    val th = 360d/n.toDouble
    val es = for (j <- 0 to 1) yield {
      val ps = for (i <- ((0 to n - 1) :+ 0)) yield {
        new Point3D(o.x + hcos(th, i, r), o.y + hsin(th, i, r), o.z + (d * j))
      }
      new Edge3D(ps.toList)
    }
    es.toList
  }

  val data06_old = makeEdge3D(List(
    List(
      (hcos_old(0),hsin_old(0),0d), (hcos_old(1),hsin_old(1),0d), (hcos_old(2),hsin_old(2),0d), (hcos_old(3),hsin_old(3),0d), (hcos_old(4),hsin_old(4),0d), (hcos_old(5),hsin_old(5),0d), (hcos_old(0),hsin_old(0),0d),
      (hcos_old(1),hsin_old(1),20d), (hcos_old(2),hsin_old(2),20d), (hcos_old(3),hsin_old(3),20d), (hcos_old(4),hsin_old(4),20d), (hcos_old(5),hsin_old(5),20d), (hcos_old(0),hsin_old(0),20d), (hcos_old(1),hsin_old(1),20d)
    ),
    List((hcos_old(1),hsin_old(1),0d), (hcos_old(1),hsin_old(1),20d)),
    List((hcos_old(2),hsin_old(2),0d), (hcos_old(2),hsin_old(2),20d)),
    List((hcos_old(3),hsin_old(3),0d), (hcos_old(3),hsin_old(3),20d)),
    List((hcos_old(4),hsin_old(4),0d), (hcos_old(4),hsin_old(4),20d)),
    List((hcos_old(5),hsin_old(5),0d), (hcos_old(5),hsin_old(5),20d)),
    List((hcos_old(1),hsin_old(1),20d), (hcos_old(1)+10d,hsin_old(1),120d)),
    List((hcos_old(2),hsin_old(2),20d), (hcos_old(2)-10d,hsin_old(1),120d)),
    List((hcos_old(4),hsin_old(4),20d), (hcos_old(4)-10d,hsin_old(1),120d)),
    List((hcos_old(5),hsin_old(5),20d), (hcos_old(5)+10d,hsin_old(1),120d)),
    List((hcos_old(2)-10d,hsin_old(1),120d), (hcos_old(5)+10d,hsin_old(1),120d))
  ))

  def rotate(p: Point3D, or: Orient): Point3D = {
    or match {
      case XY(rot) => {
        val nx = Math.cos(Math.toRadians(rot)) * p.x - Math.sin(Math.toRadians(rot)) * p.y
        val ny = Math.sin(Math.toRadians(rot)) * p.x + Math.cos(Math.toRadians(rot)) * p.y
        val nz = p.z
	new Point3D(nx, ny, nz)}
      case YZ(rot) => {
        val nx = p.x
	val ny = Math.cos(Math.toRadians(rot)) * p.y - Math.sin(Math.toRadians(rot)) * p.z 
      	val nz = Math.sin(Math.toRadians(rot)) * p.y + Math.cos(Math.toRadians(rot)) * p.z
	new Point3D(nx, ny, nz)}
    }
  }

  def rotateEdge3D(es: List[Edge3D], xy: Double, yz: Double): List[Edge3D] = {
    for (e <- es) yield {
      new Edge3D(for (p <- e.p3) yield {
        rotate(rotate(p, new XY(xy)), new YZ(yz))
      })
    }
  }

  def translate(p: Point3D, q: Point3D): Point3D = {
    new Point3D(p.x + q.x, p.y + q.y, p.z + q.z)
  }

  def translateEdge3D(es: List[Edge3D], q: Point3D): List[Edge3D] = {
    for (e <- es) yield {
      new Edge3D(for (p <- e.p3) yield {
        translate(p, q)
      })
    }
  }

  def project(p: Point3D, rot: Double): Point2D = {
    val nx = Math.cos(Math.toRadians(rot)) * p.x - Math.sin(Math.toRadians(rot)) * p.z
    val ny = p.y
    val nz = Math.sin(Math.toRadians(rot)) * p.x + Math.cos(Math.toRadians(rot)) * p.z
    val show = nz * 0.5
    new Point2D(nx + show, ny + show)
  }

  def drawD(ctx: Ctx2D, es: List[EdgeD3D], w: Window): Unit = {
    ctx.beginPath()
    for (e <- es) {
      val o = project(e.org, w.r)
      val d = project(e.dst, w.r)

      ctx.moveTo(o.x + w.x, o.y + w.y)
      ctx.lineTo(d.x + w.x, d.y + w.y)
    }
    ctx.stroke()
  }

  def draw(ctx: Ctx2D, es: List[Edge3D], w: Window): Unit = {
    ctx.beginPath()
    for (e <- es) {
      e.p3 match {
        case Nil => {}
	case h::tl => {
          val o = project(h, w.r)
          ctx.moveTo(o.x + w.x, o.y + w.y)
	  for (t <- tl) {
	    val s = project(t, w.r)
            ctx.lineTo(s.x + w.x, s.y + w.y)
	  }
	}
      }
    }
    ctx.stroke()
  }
}
