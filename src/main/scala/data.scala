package promotion.data
import promotion.wireframemodel.{Point3D, Edge3D, WireFrameModel, Poly}
object Data {
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

  val test02 = WireFrameModel.makeEdgeD3D(data01 ++ data02)

  val data03 = List(
    List((30d,30d, 30d), (30d,30d, 150d), (30d,100d, 150d), (30d,30d, 30d)),
    List((30d,30d, 30d), (30d,150d, 30d), (30d,150d, 100d), (30d,30d, 30d))
  )

  val data05 = WireFrameModel.makeRect(new Point3D(0d, 0d, 0d), 30, 6, 120) ++ 
    WireFrameModel.makeRect(new Point3D(0d, -10d, 0d), 30, 10, 6) ++
    WireFrameModel.makeRect(new Point3D(0d, -10d, 114d), 30, 10, 6)

  val test03 = WireFrameModel.makeEdge3D(data03)
  val test04 = WireFrameModel.makeRect(new Point3D(0d, 0d, 0d), 100, 100, 100)
  val test05a = WireFrameModel.translateEdge3D(data05, new Point3D(0d, 0d, -60d))
  val test05b = WireFrameModel.translateEdge3D(data05, new Point3D(0d, 0d, -60d))
  val test05c = WireFrameModel.translateEdge3D(data05, new Point3D(0d, 0d, -60d))
  def hcos_old(i: Double): Double = Math.cos(Math.toRadians(60 * i)) * 12d
  def hsin_old(i: Double): Double = Math.sin(Math.toRadians(60 * i)) * 12d
  val pl = new Poly(new Point3D(0d, 0d, 0d), 12d, 20d, 6)
  val data06 = pl.toEdge3D() ++ List(
    new Edge3D(List(pl.vrtx(true, 1), pl.vrtx(true, 5).xpls(10d).zpls(90d))),
    new Edge3D(List(pl.vrtx(true, 5), pl.vrtx(true, 5).xpls(10d).zpls(90d))),
    new Edge3D(List(pl.vrtx(true, 2), pl.vrtx(true, 4).xpls(-10d).zpls(90d))),
    new Edge3D(List(pl.vrtx(true, 4), pl.vrtx(true, 4).xpls(-10d).zpls(90d))),
    new Edge3D(List(pl.vrtx(true, 5).xpls(10d).zpls(90d), pl.vrtx(true, 4).xpls(-10d).zpls(90d)))
  )

  /*  WireFrameModel.makePoly(new Point3D(0d, 0d, 0d), 12d, 20d, 6) ++

WireFrameModel.makeEdge3D(List(
    List((hcos_old(1),hsin_old(1),0d), (hcos_old(1),hsin_old(1),20d)),
    List((hcos_old(2),hsin_old(2),0d), (hcos_old(2),hsin_old(2),20d)),
    List((hcos_old(3),hsin_old(3),0d), (hcos_old(3),hsin_old(3),20d)),
    List((hcos_old(4),hsin_old(4),0d), (hcos_old(4),hsin_old(4),20d)),
    List((hcos_old(5),hsin_old(5),0d), (hcos_old(5),hsin_old(5),20d)),
    List((hcos_old(1),hsin_old(1),20d), (hcos_old(1)+10d,hsin_old(1),120d)),
    List((hcos_old(2),hsin_old(2),20d), (hcos_old(2)-10d,hsin_old(1),120d)),
    List((hcos_old(4),hsin_old(4),20d), (hcos_old(4)-10d,hsin_old(1),120d)),
    List((hcos_old(5),hsin_old(5),20d), (hcos_old(5)+10d,hsin_old(1),120d)),
    List((hcos_old(2)-10d,hsin_old(1),120d), (hcos_old(5)+10d,hsin_old(1),120d))))
    */
}