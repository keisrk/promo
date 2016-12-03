package promotion.wireframemodel

import org.scalajs.dom.{CanvasRenderingContext2D => Ctx2D}

class Window(val r: Double, val x: Double, val y: Double)
class Point2D(val x: Double, val y: Double)
class Point3D(val x: Double, val y: Double, val z: Double)
class Edge2D(val p2: List[Point2D])
class Edge3D(val p3: List[Point3D])

class EdgeD2D(val org: Point2D, val dst: Point2D)
class EdgeD3D(val org: Point3D, val dst: Point3D)

abstract class Orient
case class XY(r: Double) extends Orient
case class YZ(r: Double) extends Orient

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

  val data01 = List(
    ((100d, 200d, 250d), (200d, 200d, 250d)),
    ((100d, 200d, 250d), (100d, 300d, 250d)),
    ((100d, 200d, 250d), (100d, 200d, 350d)),
    ((200d, 200d, 250d), (100d, 300d, 250d)),
    ((200d, 200d, 250d), (100d, 200d, 350d)),
    ((100d, 300d, 250d), (100d, 200d, 350d))
  )
  val data02 = List(
    (( 30d,  30d,  30d), (130d,  30d,  30d)),
    (( 30d,  30d,  30d), ( 30d, 130d,  30d)),
    (( 30d,  30d,  30d), ( 30d,  30d, 130d)),
    ((130d,  30d,  30d), (130d, 130d,  30d)),
    ((130d,  30d,  30d), (130d,  30d, 130d)),
    (( 30d, 130d,  30d), ( 30d, 130d, 130d)),
    (( 30d, 130d,  30d), (130d, 130d,  30d)),
    (( 30d,  30d, 130d), (130d,  30d, 130d)),
    (( 30d,  30d, 130d), ( 30d, 130d, 130d)),
    (( 30d, 130d, 130d), (130d, 130d, 130d)),
    ((130d,  30d, 130d), (130d, 130d, 130d)),
    ((130d, 130d,  30d), (130d, 130d, 130d))
  )

  val test02 = makeEdgeD3D(data01 ++ data02)

  val data03 = List(
    List((30d,30d, 30d), (30d,30d, 150d), (30d,100d, 150d), (30d,30d, 30d)),
    List((30d,30d, 30d), (30d,150d, 30d), (30d,150d, 100d), (30d,30d, 30d))
  )

  val data05 = makeRect(new Point3D(0d, 0d, 0d), 30, 6, 120) ++ 
    makeRect(new Point3D(0d, -10d, 0d), 30, 10, 6) ++
    makeRect(new Point3D(0d, -10d, 114d), 30, 10, 6)

  val test03 = makeEdge3D(data03)
  val test04 = makeRect(new Point3D(0d, 0d, 0d), 100, 100, 100)
  val test05a = translateEdge3D(data05, new Point3D(0d, 0d, -60d))
  val test05b = translateEdge3D(data05, new Point3D(0d, 0d, -60d))
  val test05c = translateEdge3D(data05, new Point3D(0d, 0d, -60d))              

  def hcos(i: Double): Double = Math.cos(Math.toRadians(60 * i)) * 12d
  def hsin(i: Double): Double = Math.sin(Math.toRadians(60 * i)) * 12d

  val data06 = makeEdge3D(List(
    List(
      (hcos(0),hsin(0),0d), (hcos(1),hsin(1),0d), (hcos(2),hsin(2),0d), (hcos(3),hsin(3),0d), (hcos(4),hsin(4),0d), (hcos(5),hsin(5),0d), (hcos(0),hsin(0),0d),
      (hcos(1),hsin(1),20d), (hcos(2),hsin(2),20d), (hcos(3),hsin(3),20d), (hcos(4),hsin(4),20d), (hcos(5),hsin(5),20d), (hcos(0),hsin(0),20d), (hcos(1),hsin(1),20d)
    ),
    List((hcos(1),hsin(1),0d), (hcos(1),hsin(1),20d)),
    List((hcos(2),hsin(2),0d), (hcos(2),hsin(2),20d)),
    List((hcos(3),hsin(3),0d), (hcos(3),hsin(3),20d)),
    List((hcos(4),hsin(4),0d), (hcos(4),hsin(4),20d)),
    List((hcos(5),hsin(5),0d), (hcos(5),hsin(5),20d)),
    List((hcos(1),hsin(1),20d), (hcos(1)+10d,hsin(1),120d)),
    List((hcos(2),hsin(2),20d), (hcos(2)-10d,hsin(1),120d)),
    List((hcos(4),hsin(4),20d), (hcos(4)-10d,hsin(1),120d)),
    List((hcos(5),hsin(5),20d), (hcos(5)+10d,hsin(1),120d)),
    List((hcos(2)-10d,hsin(1),120d), (hcos(5)+10d,hsin(1),120d))
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
