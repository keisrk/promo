package stetch.sketchtool

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.beans.property.{
  BooleanProperty => BooleanP,
  DoubleProperty => DoubleP,
  StringProperty => StringP}
import scalafx.animation.AnimationTimer
import scalafx.scene.paint.Color._
import scalafx.scene.image.{Image}
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.SubScene
import scalafx.scene.{Scene}
import scalafx.scene.canvas.{Canvas}
import scalafx.scene.layout.{VBox, HBox}
import scalafx.scene.shape.{Shape, Rectangle}

import sketch.fxio3d.{FxIO3D}
import sketch.animation.Clock
import sketch.windowMaker.WindowMaker
import sketch.flowChart.{FlowChart}
import sketch.component.{Position, Direction}
import sketch.svgFx.{SVGFx}


object Test {
val test = List(
  ("width", DoubleP(300d), 600), ("height", DoubleP(300d), 600), ("theta", DoubleP(60d), 360), ("duration", DoubleP(60d), 720), 
  ("x", DoubleP(60d), 300), ("y", DoubleP(60d), 300), ("z", DoubleP(60d), 300))
val j_shape = 
"""
[
{"id": "v0", "shape": "vec", "data": [[-25, 5, 5], [25, 25, 25]]}
]
"""
val j_trans =
"""
[
    {"id": "A-v0", "tr": [[0, 0, 0], [15, 0, 0]]},
    {"id": "B-v0", "tr": [[15, 0, 0], [0, 0, 0]]},
    {"id": "C-v0", "tr": [[0, 0, 0], [0, 0, 0]], "rz": [0, 15]},
    {"id": "D-v0", "tr": [[0, 0, 0], [0, 0, 0]], "rz": [15, 0]}
]
"""
val j_sh =
"""
[
    {"id": "v0", "shape": "vec", "data": [[0, 200, 0], [110, 180, 90]]},
    {"id": "v1", "shape": "vec", "data": [[320, 200, 0], [110, 180, 90]]},
    {"id": "bl", "shape": "ply", "d": 120, "seq": [[-100, 380, 0], [200, 380, 0], [400, 440, 0], [400, 460, 0]]},
    {"id": "br", "shape": "ply", "d": 120, "seq": [[-100, 200, 0], [200, 200, 0], [400, 140, 0], [400, 120, 0]]},
    {"shape": "ply", "seq": [[0, 0, 0], [100, 0, 0], [0, 0, 0], [0, 100, 0], [0, 0, 0], [0, 0, 100]]},
    {"id": "ca", "shape": "vec", "data": [[506, 150, 6], [12, 55, 12]]},
    {"id": "cb", "shape": "vec", "data": [[666, 150, 6], [12, 55, 12]]},
    {"id": "cc", "shape": "vec", "data": [[506, 380, 6], [12, 55, 12]]},
    {"id": "cd", "shape": "vec", "data": [[666, 380, 6], [12, 55, 12]]},

    {"shape": "cmp", "data": [{"shape": "vec", "data": [[500,150,0], [24, 50, 24]]},
                              {"shape": "vec", "data": [[660,150,0], [24, 50, 24]]},
                              {"shape": "vec", "data": [[500,380,0], [24, 50, 24]]},
                              {"shape": "vec", "data": [[660,380,0], [24, 50, 24]]}]}
]
"""
val j_tr =
"""
[
    {"id": "A-v0", "tr": [[666, 0, 0], [506, 0, 0]]},
    {"id": "B-v0", "tr": [[506, 0, 0], [506, 0, 0]]},
    {"id": "C-v0", "tr": [[506, 0, 0], [346, 0, 0]], "rz": [0, 15]},
    {"id": "D-v0", "tr": [[346, 0, 0], [ 86, 0, 0]], "rz": [15, 0]},

    {"id": "A-v1", "tr": [[666, 0, 0], [506, 0, 0]]},
    {"id": "B-v1", "tr": [[506, 0, 0], [346, 0, 0]]},
    {"id": "C-v1", "tr": [[346, 0, 0], [346, 0, 0]]},
    {"id": "D-v1", "tr": [[346, 0, 0], [346, 0, 0]]},

    {"id": "A-ca", "tr": [[0, 50, 0], [0, 50, 0]]},
    {"id": "B-ca", "tr": [[0, 50, 0], [0,  0, 0]]},
    {"id": "C-ca", "tr": [[0,  0, 0], [0,  0, 0]]},
    {"id": "D-ca", "tr": [[0,  0, 0], [0, 50, 0]]},

    {"id": "A-cb", "tr": [[0,  0, 0], [0,  0, 0]]},
    {"id": "B-cb", "tr": [[0,  0, 0], [0, 50, 0]]},
    {"id": "C-cb", "tr": [[0, 50, 0], [0, 50, 0]]},
    {"id": "D-cb", "tr": [[0, 50, 0], [0,  0, 0]]},

    {"id": "A-cc", "tr": [[0, -50, 0], [0, -50, 0]]},
    {"id": "B-cc", "tr": [[0, -50, 0], [0,   0, 0]]},
    {"id": "C-cc", "tr": [[0,   0, 0], [0,   0, 0]]},
    {"id": "D-cc", "tr": [[0,   0, 0], [0, -50, 0]]},

    {"id": "A-cd", "tr": [[0,   0, 0], [0,   0, 0]]},
    {"id": "B-cd", "tr": [[0,   0, 0], [0, -50, 0]]},
    {"id": "C-cd", "tr": [[0, -50, 0], [0, -50, 0]]},
    {"id": "D-cd", "tr": [[0, -50, 0], [0,   0, 0]]}
]
"""

val demoSh =
"""
[
    {"id": "cnv", "shape": "cmp", "data": [
        {"id": "cnv0", "shape": "vec", "data": [[-200, 0, 124], [71, 115, 12]]},
        {"id": "cnv1", "shape": "vec", "data": [[-400, 0, 124], [71, 115, 12]]},
        {"id": "cnv2", "shape": "vec", "data": [[-600, 0, 124], [71, 115, 12]]}
    ]},

    {"id": "push", "shape": "ply", "d": 124, "seq": [[155, 195, 0], [170, 195, 0], [170, 95, 0], [0, 95, 0], [0, -95, 0], [170, -95, 0], [170, -195, 0], [155, -195, 0]]},
    {"id": "cylNF", "shape": "cmp", "data": [
        {"id": "caseN", "shape": "cmp", "data": [
            {"id": "worksN", "shape": "cmp", "data": [
                {"id": "workN0", "shape": "vec", "data": [[0, 0,  0], [71, 115, 12]]},
                {"id": "workN1", "shape": "vec", "data": [[0, 0, 12], [71, 115, 12]]},
                {"id": "workN2", "shape": "vec", "data": [[0, 0, 24], [71, 115, 12]]},
                {"id": "workN3", "shape": "vec", "data": [[0, 0, 36], [71, 115, 12]]},
                {"id": "workN4", "shape": "vec", "data": [[0, 0, 48], [71, 115, 12]]}
            ]},
            {"id": "caseNn", "shape": "ply", "d": 124, "seq": [[0, -10, 0], [20, 0, 0], [126, 0, 0]]},
            {"id": "caseNf", "shape": "ply", "d": 124, "seq": [[0, 134, 0], [20, 124, 0],[126, 124, 0]]}
        ]},
        {"id": "caseF", "shape": "cmp", "data": [
            {"id": "worksF", "shape": "cmp", "data": [
                {"id": "workF0", "shape": "vec", "data": [[0, 0,  0], [71, 115, 12]]},
                {"id": "workF1", "shape": "vec", "data": [[0, 0, 12], [71, 115, 12]]},
                {"id": "workF2", "shape": "vec", "data": [[0, 0, 24], [71, 115, 12]]},
                {"id": "workF3", "shape": "vec", "data": [[0, 0, 36], [71, 115, 12]]},
                {"id": "workF4", "shape": "vec", "data": [[0, 0, 48], [71, 115, 12]]}
            ]},
            {"id": "caseFn", "shape": "ply", "d": 124, "seq": [[0, -10, 0], [20, 0, 0], [126, 0, 0]]},
            {"id": "caseFf", "shape": "ply", "d": 124, "seq": [[0, 134, 0], [20, 124, 0],[126, 124, 0]]}
        ]}
    ]}
]
"""

val demoTrans =
"""
[
    {"id": "A-cnv", "tr":[[-200, 0, 0], [0, 0, 0]]},
    {"id": "B-cnv", "tr":[[-200, 0, 0], [0, 0, 0]]},
    {"id": "C-cnv", "tr":[[-200, 0, 0], [0, 0, 0]]},
    {"id": "D-cnv", "tr":[[-200, 0, 0], [0, 0, 0]]},
    {"id": "E-cnv", "tr":[[-200, 0, 0], [0, 0, 0]]},
    {"id": "F-cnv", "tr":[[-200, 0, 0], [0, 0, 0]]},
    {"id": "G-cnv", "tr":[[-200, 0, 0], [0, 0, 0]]},
    {"id": "H-cnv", "tr":[[-200, 0, 0], [0, 0, 0]]},
    {"id": "I-cnv", "tr":[[-200, 0, 0], [0, 0, 0]]},

    {"id": "A-push", "tr":[[-170, 0, 0], [-170, 0, 0]]},
    {"id": "B-push", "tr":[[-170, 0, 0], [-170, 0, 0]]},
    {"id": "C-push", "tr":[[-170, 0, 0], [-170, 0, 0]]},
    {"id": "D-push", "tr":[[-170, 0, 0], [-170, 0, 0]]},
    {"id": "E-push", "tr":[[-170, 0, 0], [0, 0, 0]]},
    {"id": "F-push", "tr":[[0, 0, 0], [-170, 0, 0]]},
    {"id": "G-push", "tr":[[-170, 0, 0], [-170, 0, 0]]},
    {"id": "H-push", "tr":[[-170, 0, 0], [0, 0, 0]]},
    {"id": "I-push", "tr":[[0, 0, 0], [-170, 0, 0]]},

    {"id": "A-worksN", "tr":[[0, 0, -1000], [0, 0, -1000]]},
    {"id": "B-workN0", "tr":[[0, 0, 124], [0, 0, 106]]},
    {"id": "B-workN1", "tr":[[0, 0, 112], [0, 0, 100]]},
    {"id": "B-workN2", "tr":[[0, 0, 100], [0, 0,  94]]},
    {"id": "B-workN3", "tr":[[0, 0, 88], [0, 0, 88]]},
    {"id": "B-workN4", "tr":[[0, 0, 76], [0, 0, 82]]},
    {"id": "C-workN0", "tr":[[0, 0, 106], [0, 0, 88]]},
    {"id": "C-workN1", "tr":[[0, 0, 100], [0, 0, 88]]},
    {"id": "C-workN2", "tr":[[0, 0,  94], [0, 0, 88]]},
    {"id": "C-workN3", "tr":[[0, 0,  88], [0, 0, 88]]},
    {"id": "C-workN4", "tr":[[0, 0,  82], [0, 0, 88]]},
    {"id": "D-worksN", "tr":[[0, 0, 88], [0, 0, 88]]},
    {"id": "E-worksN", "tr":[[0, 0, 88], [170, 0, 88]]},
    {"id": "F-worksN", "tr":[[170, 0, 88], [170, -300, 88]]},
    {"id": "G-worksN", "tr":[[0, 0, -1000], [0, 0, -1000]]},
    {"id": "H-workN0", "tr":[[0, 0, 124], [0, 0, 106]]},
    {"id": "H-workN1", "tr":[[0, 0, 112], [0, 0, 100]]},
    {"id": "H-workN2", "tr":[[0, 0, 100], [0, 0,  94]]},
    {"id": "H-workN3", "tr":[[0, 0, 88], [0, 0, 88]]},
    {"id": "H-workN4", "tr":[[0, 0, 76], [0, 0, 82]]},
    {"id": "I-workN0", "tr":[[0, 0, 106], [0, 0, 88]]},
    {"id": "I-workN1", "tr":[[0, 0, 100], [0, 0, 88]]},
    {"id": "I-workN2", "tr":[[0, 0,  94], [0, 0, 88]]},
    {"id": "I-workN3", "tr":[[0, 0,  88], [0, 0, 88]]},
    {"id": "I-workN4", "tr":[[0, 0,  82], [0, 0, 88]]},


    {"id": "A-worksF", "tr":[[0, 0, -1000], [0, 0, -1000]]},
    {"id": "B-worksF", "tr":[[0, 0, -1000], [0, 0, -1000]]},
    {"id": "C-worksF", "tr":[[0, 0, -1000], [0, 0, -1000]]},
    {"id": "D-worksF", "tr":[[0, 0, -1000], [0, 0, -1000]]},

    {"id": "E-workF0", "tr":[[0, 0, 124], [0, 0, 106]]},
    {"id": "E-workF1", "tr":[[0, 0, 112], [0, 0, 100]]},
    {"id": "E-workF2", "tr":[[0, 0, 100], [0, 0,  94]]},
    {"id": "E-workF3", "tr":[[0, 0, 88], [0, 0, 88]]},
    {"id": "E-workF4", "tr":[[0, 0, 76], [0, 0, 82]]},
    {"id": "F-workF0", "tr":[[0, 0, 106], [0, 0, 88]]},
    {"id": "F-workF1", "tr":[[0, 0, 100], [0, 0, 88]]},
    {"id": "F-workF2", "tr":[[0, 0,  94], [0, 0, 88]]},
    {"id": "F-workF3", "tr":[[0, 0,  88], [0, 0, 88]]},
    {"id": "F-workF4", "tr":[[0, 0,  82], [0, 0, 88]]},
    {"id": "G-worksF", "tr":[[0, 0, 88], [0, 0, 88]]},
    {"id": "H-worksF", "tr":[[0, 0, 88], [170, 0, 88]]},
    {"id": "I-worksF", "tr":[[170, 0, 88], [170, -300, 88]]},

    {"id": "A-caseN", "tr":[[0, -142, 0], [0, -142, 0]]},
    {"id": "B-caseN", "tr":[[0, -142, 0], [0, -142, 0]]},
    {"id": "C-caseN", "tr":[[0, -142, 0], [0, -142, 0]]},
    {"id": "D-caseN", "tr":[[0, -142, 0], [0, -142, 0]]},
    {"id": "E-caseN", "tr":[[0, -142, 0], [0, -142, 0]]},
    {"id": "F-caseN", "tr":[[0, -142, 0], [0, -142, 0]]},
    {"id": "G-caseN", "tr":[[0, -142, 0], [0, -142, 0]]},
    {"id": "H-caseN", "tr":[[0, -142, 0], [0, -142, 0]]},
    {"id": "I-caseN", "tr":[[0, -142, 0], [0, -142, 0]]},

    {"id": "A-cylNF", "tr":[[0, 80, 0], [0, 80, 0]]},
    {"id": "B-cylNF", "tr":[[0, 80, 0], [0, 80, 0]]},
    {"id": "C-cylNF", "tr":[[0, 80, 0], [0, 80, 0]]},
    {"id": "D-cylNF", "tr":[[0, 80, 0], [0, -80, 0]]},
    {"id": "E-cylNF", "tr":[[0, -80, 0], [0, -80, 0]]},
    {"id": "F-cylNF", "tr":[[0, -80, 0], [0, -80, 0]]},
    {"id": "G-cylNF", "tr":[[0, -80, 0], [0, 80, 0]]},
    {"id": "H-cylNF", "tr":[[0, 80, 0], [0, 80, 0]]},
    {"id": "I-cylNF", "tr":[[0, 80, 0], [0, 80, 0]]},

    {"id": "A-root", "tr":[[300, 0, 230], [300, 0, 230]]},
    {"id": "B-root", "tr":[[300, 0, 230], [300, 0, 230]]},
    {"id": "C-root", "tr":[[300, 0, 230], [300, 0, 230]]},
    {"id": "D-root", "tr":[[300, 0, 230], [300, 0, 230]]},
    {"id": "E-root", "tr":[[300, 0, 230], [300, 0, 230]]},
    {"id": "F-root", "tr":[[300, 0, 230], [300, 0, 230]]},
    {"id": "G-root", "tr":[[300, 0, 230], [300, 0, 230]]},
    {"id": "H-root", "tr":[[300, 0, 230], [300, 0, 230]]},
    {"id": "I-root", "tr":[[300, 0, 230], [300, 0, 230]]}
]
"""

val demoV =List(
    List(("0", "Start", 0), ("A", "製品到着", 0),("B", "1St.開 & 2St.閉", 1),("C","姿勢制御", 2), ("D", "センサON", 3)),
    List(("NA", "", 0),     ("NA", "", 0),      ("E", "検査NG", 3),         ("F", "非常停止", 4)))
val demoE = List(("0", "A"), ("A", "B"), ("B", "C"), ("C", "D"), ("D", "A"), ("D_else", "E"), ("E", "C"), ("E_else", "F"))
val shusekiV = List(
//  List(("NA", "", 0), ("D", "手前", 0), ("E", "排出シリンダ出", 0), ("F", "排出シリンダ戻", 0)),
  List(("0", "起動", 0), ("A", "集積<5", 3), ("B", "製品到着", 0), ("C", "シリンダ下降", 0)),
  List(("NA", "", 0), ("DG", "手前・奥", 0), ("EH", "排出シリンダ出", 0), ("FI", "排出シリンダ戻", 0))
  )
val shusekiE = List(("0", "A"), ("A", "B"), ("A_else", "DG"), ("B", "C"), ("C", "A"), ("DG", "EH"), ("EH", "FI"), ("FI", "A"))
}

object Main extends JFXApp {
  val fc = List(("A", "B", 0), ("B", "C", 0), ("C", "D", 0), ("D", "E", 0), ("E", "F", 0), ("F", "G", 0), ("G", "H", 0), ("H", "I", 0), ("I", "D", 0))
  val qs = new FlowChart("A", fc) 
  val img = new SubScene(700, 700){
  }
  val svg = new SVGFx(img)
  val v = svg.arrange(Test.shusekiV  , 200, 120)
  svg.draw(v, Test.shusekiE)

  def draw(cnv: Canvas,  shape: String, trans: String): Unit = {
    val l = List("A", "B", "C", "D", "E", "F")
    val q = qs.st8//(x: Int) => l(x % l.length)
    val ctx = cnv.graphicsContext2D; ctx.scale(1, -1); ctx.translate(0, -cnv.height.toDouble)
    val p = new FxIO3D(ctx)
    val cl = new Clock(100, q)
    val j_sh = p.load(shape)
    val j_tr = p.load_trans(trans)
    val es = p.mashup(j_sh, j_tr, cl.state(), 1d)
    p.draw(es, (0d, 0d, 0d))   
    val timer = AnimationTimer(t => {
      ctx.clearRect(0, 0, cnv.width.toDouble, cnv.height.toDouble)
      cl.incl(1d)
      val es = p.mashup(j_sh, j_tr, cl.state(), cl.inter())
      p.draw(es, (0d, 0d, 0d))
      svg.setColor(cl.state())
    })
    timer.start()
  }
  val icon = new Image(getClass.getResourceAsStream("/images/logo.png"))
  /*
  val cola = new VBox {
    val cnv0 = new Canvas(100, 100); val cnv1 = new Canvas(100, 100); val cnv2 = new Canvas(100, 100)
    val _id0 = "test"; val pos0 = new Position(DoubleP(100d), DoubleP(100d), DoubleP(100d)); val dir0 = new Direction(DoubleP(100d), DoubleP(100d), DoubleP(100d))
    val _id1 = "test"; val pos1 = new Position(DoubleP(100d), DoubleP(100d), DoubleP(100d)); val dir1 = new Direction(DoubleP(100d), DoubleP(100d), DoubleP(100d))
    val _id2 = "test"; val pos2 = new Position(DoubleP(100d), DoubleP(100d), DoubleP(100d)); val dir2 = new Direction(DoubleP(100d), DoubleP(100d), DoubleP(100d))
    draw(cnv0, Test.j_shape, Test.j_trans); draw(cnv1, Test.j_shape, Test.j_trans); draw(cnv2, Test.j_shape, Test.j_trans)
    val cntr0 = WindowMaker.makeCnvControl(_id0, dir0, pos0, cnv0)
    val cntr1 = WindowMaker.makeCnvControl(_id1, dir1, pos1, cnv1)
    val cntr2 = WindowMaker.makeCnvControl(_id2, dir2, pos2, cnv2)
    spacing = 25
    children = List(cntr0, cntr1, cntr2) }*/
  stage = new JFXApp.PrimaryStage {
    title.value = "Sketch Tool"
    icons += icon
  }
  val bord = new BorderPane {
    val cnv = new Canvas(1200, 500)
    draw(cnv, Test.demoSh, Test.demoTrans)//Test.j_sh, Test.j_tr)
    //left = cola
    left = img
    center = cnv
  }
  stage.scene = new Scene{
    root = bord
  }
}
