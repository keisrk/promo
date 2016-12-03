package promotion.data
import promotion.wireframemodel.{Point3D, WireFrameModel}
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
}