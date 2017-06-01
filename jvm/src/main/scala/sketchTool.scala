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
import sketch.controlFlowGraph.{ControlFlowGraph}
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
}

object Main extends JFXApp {
  val cfg = List(("A", "B", 1), ("B", "C", 2), ("C", "D", 0), ("D", "A", 1))
  val qs = new ControlFlowGraph("A", cfg) 
  val img = new SubScene(300, 700){
  }
  val svg = new SVGFx(img)
  //img.content = svg.test
  val lbl = List("製品到着","1St.開 & 2St.閉","姿勢制御","1St.閉 & 2St.開 ")
  for ((s, i) <- lbl.zipWithIndex) cfg(i) match {
    case (q, p, lbl) => svg.makeLabel(q, s, 150, (i+1) * 120, lbl)
  }
  def draw(cnv: Canvas,  shape: String, trans: String): Unit = {
    val l = List("A", "B", "C", "D")
    val q = qs.st8//(x: Int) => l(x % l.length)
    val ctx = cnv.graphicsContext2D; ctx.scale(1, -1); ctx.translate(0/*- cnv.width.toDouble / 2*/, -cnv.height.toDouble)
    val p = new FxIO3D(ctx)
    val cl = new Clock(100, q)
    val j_sh = p.load(shape)
    val j_tr = p.load_trans(trans)
    val es = p.mashup(j_sh, j_tr, cl.state(), 1d)
    p.draw(es, (0d, 0d, 0d))   
    val timer = AnimationTimer(t => {
      ctx.clearRect(/*- cnv.width.toDouble / 2*/0, 0, cnv.width.toDouble, cnv.height.toDouble)
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
    draw(cnv, Test.j_sh, Test.j_tr)
    //left = cola
    left = img
    center = cnv
    //bottom = img
  }
  stage.scene = new Scene{
    root = bord
  }
}
